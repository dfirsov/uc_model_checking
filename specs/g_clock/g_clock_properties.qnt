module g_clock_properties {

    import basicSpells.* from "../../spells/basicSpells"
    import uc.* from "./uc"
    import g_clock_types as g_clock_types from "./g_clock_types" 
    import g_clock_ideal as g_clock from "./g_clock_ideal"



    val P = Set(1, 2, 3)
    val C = Set(3)
    val SIDS = Set(1, 2)

    action step : bool = any {
        g_clock::step,  // internal dynamics of the g_clock
        env_send_reqs, // sends interface-level requests
        env_consume_responses, // consumes interface-level responses
    }


    action init = {
        g_clock::init(C,0,Set())
    }

 

    // Environment interaction: non-deterministically add requests when input is empty
    action env_send_reqs : bool = all {
        // Precondition: input must be empty
        g_clock::s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet party = P.oneOf()
        nondet sid = SIDS.oneOf()
        val partySID = {party: party, sid: sid}
        
        // Non-deterministically choose one of the three request types
        any {
            // Option 1: RegisterRequest
            g_clock::s' = {...g_clock::s, 
                input: Set(g_clock_types::RegisterRequest({nonce: g_clock::s.nonce,party: partySID})),
                nonce: g_clock::s.nonce + 1

            },
            // Option 2: TimeRequest
            g_clock::s' = {...g_clock::s, 
                input: Set(g_clock_types::TimeRequest({nonce: g_clock::s.nonce, party: partySID})),
                nonce: g_clock::s.nonce + 1
            },

            // Option 3: OkayRequest
            g_clock::s' = {...g_clock::s, 
                input: Set(g_clock_types::OkayRequest({nonce: g_clock::s.nonce, party: partySID})),
                nonce: g_clock::s.nonce + 1
            },
        }
    }


    pure def env_consume_response(sys: g_clock_types::System) : Set[g_clock_types::Transition] = {
         sys.input.g_clock_types::get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                g_clock_types::LogMessage(msg_log),
                g_clock_types::UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        env_consume_response(g_clock::s).size() > 0,

        // Non-deterministically choose one response message to consume
        nondet transition = env_consume_response(g_clock::s).oneOf()

        // Update the system state by consuming the message    
        g_clock::s' = g_clock::process_transitions(g_clock::s, transition)
        
    }



// must fail
val LogGrows = 
    (g_clock::s.log.size() < 5)

// must fail
val RegSize = g_clock::s.state.Reg.size() < 2

// must fail
val TimeAdvances = g_clock::s.state.TIME < 2

// must fail
val HandlesLiveness = 
    g_clock::s.SIM_handles.size() < 2


// system is well-defined
val OneMsg = 
    g_clock::s.input.size() <= 1


// Property 1 (Time Agreement): If two parties query the time and no OK requests have been sent in the middle then G-clock responds with the same time.
val TimeAgreement = g_clock::s.log.forall(r1 =>
    match r1.msg {
      | TimeResponse(t1) =>
          g_clock::s.log.forall(r2 =>
            match r2.msg {
              | TimeResponse(t2) =>
                   (r2.idx > r1.idx) implies {
                    // Check that no OK requests in between
                    val no_okay_in_between = g_clock::s.log.forall(rm =>
                      (rm.idx > r1.idx and rm.idx < r2.idx) implies {
                        match rm.msg {
                          | OkayRequest(_) => false
                          | _ => true
                        }
                      } 
                    )

                    no_okay_in_between implies {
                      t1.time == t2.time
                    } 
                  } 
              | _ => true
            }
          )
      | _ => true
    }
  )

// Property 2 (Time Liveness):  If all registered parties are ready, then time advances.
// this is non-trivial since we have to track who was registered and corrupted at all times
val TimeLiveness = g_clock::s.log.forall(r1 =>
   g_clock::s.log.forall(r2 =>
             (r2.idx == r1.idx + 1) implies {
              match r2.msg {
                | OkayRequest(p) => {
                      // check if all registered parties were ready at r1
                    val all_ready = r1.state.Reg.forall(rp => or {
                     r1.state.ok.contains(rp), 
                     rp == p.party, 
                     p.party.party.in(g_clock::s.params.C)
                    })

                      if (r2.state.Reg.contains(p.party) and all_ready) {
                        r2.state.TIME == r1.state.TIME + 1
                      } else {
                        r2.state.TIME == r1.state.TIME
                      }
                }
                | _ => r2.state.TIME == r1.state.TIME
              }
            } 
          )
  )  

// Property 3 (Time Soundness): If time advanced then all currently registered parties were ready.
// this is boring since we have a full control of when to advance time
val TimeSoundness = g_clock::s.log.forall(r1 =>
   g_clock::s.log.forall(r2 =>
            (r2.idx == r1.idx + 1 and r2.state.TIME > r1.state.TIME) implies {
              match r2.msg {
                | OkayRequest(p) =>
                      // time advanced, check that all registered parties were ready
                    val all_ready = r1.state.Reg.forall(rp => or {
                        r1.state.ok.contains(rp), 
                        rp == p.party, 
                        p.party.party.in(g_clock::s.params.C)
                    })

                    and {
                      all_ready,
                      r2.state.TIME == r1.state.TIME + 1,
                      //r2.ok == Set(),
                    }
                | _ => false
              }
            } 
          )
  )  
    
}