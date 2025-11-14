module f_ac_properties {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"

    import uc.* from "../uc"
    import g_clock_types as gct from "../g_clock/g_clock_types"
    import g_clock as gccc from "../g_clock/g_clock"    
    import f_ac_types as fct  from "./f_ac_types"
    import f_ac as fccc  from "./f_ac"
    


    val P = Set(1,2)
    val C = Set()
    val sids = Set(1)
    val delta: int = 1
/*
Simple setting:
- initialize both functionalities
- if none of the systems have input 
  - then non-deterministically add requests to either functionality
  - give control to SIM at clock or at f_ac
- when f_ac has response, consume it
*/    
    

    action step : bool = any {

        all {
            fccc::step,  
            //gccc::s' = gccc::s
        },
        all{
            fccc::f_ac_s' = fccc::f_ac_s,
            gccc::step,
        },
        all{
            env_send_reqs,
            gccc::stutter
        },
        all {
            env_consume_responses,
            gccc::stutter
        }

        

    }

    action init = all {
        fccc::init(P, C, delta, sids),
        gccc::init(C,0,Set({party: 1, sid: 1}, {party:2, sid:1})),
    }

 

    // // Environment interaction: non-deterministically add requests when input is empty
    action env_send_reqs : bool = all {
        // Precondition: input must be empty
        gccc::s.input.size() == 0 and fccc::f_ac_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet party = P.oneOf()
        nondet sid = sids.oneOf()
        nondet data = Set(42,43).oneOf()
        val partySID = {party: party, sid: sid}
        val cfg = {party: party, sid: sid, nonce: fccc::f_ac_s.nonce}

        // Non-deterministically choose one of the three request types
        any {
            // Option 1: Send
            fccc::f_ac_s' = {...fccc::f_ac_s, 
                input: Set(fct::SendRequest({cfg: cfg, Q: party, x: data})),
                nonce: fccc::f_ac_s.nonce + 1

            },
            // Option 2: Fetch
            fccc::f_ac_s' = {...fccc::f_ac_s, 
                input: Set(fct::FetchRequest({cfg: cfg})),
                nonce: fccc::f_ac_s.nonce + 1

            },

            // Option 3: OK
            fccc::f_ac_s' = {...fccc::f_ac_s, 
                input: Set(fct::OKRequest({cfg: cfg})),
                nonce: fccc::f_ac_s.nonce + 1

            },
        }
    }


    pure def env_consume_response(sys: fct::System) : Set[fct::Transition] = {
         sys.input.fct::get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                fct::LogMessage(msg_log),
                fct::UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(fccc::f_ac_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        fccc::f_ac_s' = fccc::process_transitions(fccc::f_ac_s, transition)
    }



// must fail
val P1 = fccc::f_ac_s.input.size() == 0
val P2 = fccc::f_ac_s.log.size() <= 7
val P3 = fccc::f_ac_s.log.map(x => x.msg).fct::get_ok_response_messages().size() <= 2
val P4 = fccc::f_ac_s.log.map(x => x.msg).fct::get_fetch_response_messages().size() <= 2
val P5 = fccc::f_ac_s.log.map(x => x.msg).fct::get_send_response_messages().size() <= 2
val P6 = gccc::s.input.size() == 0
val P7 = gccc::s.log.map(x => x.msg).gct::get_all_response_messages().size() == 0
val P8 = gccc::s.state.TIME <= 4
val P9 = fccc::f_ac_s.log.map(x => x.msg).fct::get_fetch_response_messages().forall(m => m.data.size() == 0)


// ∆-delayed Liveness: if P sends a message x to Q in some round t, and Q attempts to fetch in some 
// round t‘ ≥ t + ∆ then Q receives x in some round t’’ ∈ [t, t’].  
val DDL = {
    fccc::f_ac_s.log.forall(msg1 =>
        match msg1.msg {
            | SendResponse(p) => { 
                val fetch_response_exists = fccc::f_ac_s.log.exists(msg3 => {
                    match msg3.msg {
                        | FetchResponse(d) => {
                            d.t_prim >= p.t + delta and p.Q == d.cfg.party
                        }
                        | _ => false
                    }
                })

                fetch_response_exists implies {
                    fccc::f_ac_s.log.exists(msg3 => {
                        match msg3.msg {
                            | FetchResponse(d) => {
                                (p.x.in(d.data))
                                //true //d.t_prim - p.t <= delta and p.Q == d.cfg.party and p.x.in(d.data)
                            }
                            | _ => false
                        }
                    })
                }

                
            }
            | _ => true
        }

    )    
}

// Authentication: If a nonempty set of messages was delivered from P to Q then P indeed sent that message to Q in the past.
val Auth = fccc::f_ac_s.log.forall(log_entry1 => {
  match log_entry1.msg {
    | FetchResponse(p) => { // 
        // find corresponding FetchRequest in the log
        (p.data != Set()) implies fccc::f_ac_s.log.exists(log_entry2 => {
          match log_entry2.msg {
            | SendRequest(q) => {
                q.Q == p.cfg.party and q.x.in(p.data)
              }
            | _ => false
          }
        })
      }
    | _ => true
  }
})


// Order preservation: 
val Order = fccc::f_ac_s.log.forall(log_entry1 => {
  match log_entry1.msg {
    | SendResponse(p) => { // 
        // find corresponding FetchRequest in the log
        fccc::f_ac_s.log.forall(log_entry2 => {
            match log_entry2.msg {
                | SendResponse(q) => {
                        and {
                            log_entry1.idx < log_entry2.idx, // second send response happens later
                            p.Q == q.Q, // data being sent to the same receipient
                            p.x != q.x // data is different
                            } implies {
                            fccc::f_ac_s.log.forall(log_entry3 => {
                                match log_entry3.msg {
                                    | FetchResponse(d) => {
                                        q.x.in(d.data) implies {
                                            fccc::f_ac_s.log.exists(log_entry4 => {
                                                match log_entry4.msg {
                                                    | FetchResponse(b) => {
                                                        and {
                                                            //true
                                                            p.x.in(b.data) and log_entry4.idx <= log_entry3.idx                   
                                                        }
                                                    }
                                                    | _ => false
                                                }
                                            })
                                        }
                                    }
                                    | _ => true
                                }
                            })
                        }
                    }
                | _ => true
            }
        })
      }
    | _ => true
  }
}) 

}