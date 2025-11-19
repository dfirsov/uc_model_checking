module f_ac_properties {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"    
    import f_ac_types.*   from "./f_ac_types"
    import f_ac.*  from "./f_ac"
    

    val Parties = Set(1,2)
    val C = Set()
    val sids = Set(1)
    val delta_param: int = 2   


    action step : bool = any {
        fac_step,  
        gc_step_fac_wait,
        env_fac_send_all_reqs,
        env_consume_responses,
    }

    action gc_step_fac_wait : bool =  all {
        fac_s' = fac_s,
        gc_step,        
    }

    action init = all {
        fac_init(Parties, C, delta_param, sids),
        gc_init(C,0,Set({party: 1, sid: 1}, {party:2, sid:1})),
    }

    action env_fac_send_all_reqs : bool = all {
        nondet from = Parties.oneOf()
        nondet top = Parties.oneOf()
        nondet sid = sids.oneOf()

        any {
            env_fac_send_reqs(from, top, sid),
            env_fac_fetch_reqs(from, sid),
            env_ok_reqs(from, sid),
            env_fac_sim_reqs,
            env_gc_sim_reqs,
        }
    }

 
    // // Environment interaction: non-deterministically add requests when input is empty
    action env_fac_send_reqs(from:Party, top: Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and fac_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet data = Set(42,43,44,45).oneOf()
        val cfg = {party: from, sid: sid, nonce: fac_s.nonce}

        fac_s' = {...fac_s, 
            input: Set(SendRequest({cfg: cfg, Q: top, x: data})),
            nonce: fac_s.nonce + 1

        },
        gc_stutter
    }




    action env_fac_fetch_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and fac_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: fac_s.nonce}

        // Option 2: Fetch
        fac_s' = {...fac_s, 
            input: Set(FetchRequest({cfg: cfg})),
            nonce: fac_s.nonce + 1

        },
        gc_stutter   
    }

    action env_ok_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and fac_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: fac_s.nonce}

        // Option 3: OK
        fac_s' = {...fac_s, 
            input: Set(OKRequest({cfg: cfg})),
            nonce: fac_s.nonce + 1

        },   
        gc_stutter     
    }

    action env_fac_sim_reqs : bool = all {
        gc_s.input.size() == 0 and fac_s.input.size() == 0,
        fac_s' = {...fac_s, 
            input: Set(SIMControl),
            nonce: fac_s.nonce + 1

        },
        gc_stutter
    }

    action env_gc_sim_reqs : bool = all {
        gc_s.input.size() == 0 and fac_s.input.size() == 0,
        gc_s' = {...gc_s, 
            input: Set(GC_SIMControl),
            nonce: gc_s.nonce + 1
        },
        fac_s' = fac_s,
    }

    pure def env_consume_response(sys: FAC_System) : Set[FAC_Transition] = {
         sys.input.fac_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                FAC_LogMessage(msg_log),
                FAC_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(fac_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        fac_s' = fac_process_transitions(fac_s, transition),
        gc_stutter
    }

 
    // quint test f_ac_properties.qnt --match test1
    // init -> send -> ok -> fetch 
    run test1 = {

        val party1 = 1
        val party2 = 2
        val sid = 1

        init.then(env_fac_send_reqs(party1, party2, sid))
        .then(fac_step_gc_wait_with(fac_send_1))
        .then(fac_step_gc_wait_with(fac_send_2))
        .then(fac_step_gc_wait_with(fac_send_3))
        .then(fac_gc_step_with(fac_send_4))
        .then(fac_wait_gc_step_with(gc_okay_1))
        .then(fac_wait_gc_step_with(gc_okay_2))
        .then(fac_wait_gc_step_with(gc_okay_3))
        .then(fac_wait_gc_step_with(gc_okay_4))        
        .then(fac_gc_step_with(fac_send_5))
        .then(fac_step_gc_wait_with(fac_send_6))
        .then(env_consume_responses)
        .expect(fac_s.log.map(x => x.msg).fac_get_send_response_messages().size() == 1)
        .then(env_ok_reqs(party2, sid))        
        .then(fac_gc_step_with(fac_ok_1))
        .then(fac_wait_gc_step_with(gc_okay_1))
        .then(fac_wait_gc_step_with(gc_okay_2))
        .then(fac_wait_gc_step_with(gc_okay_3))
        .then(fac_wait_gc_step_with(gc_okay_4))          
        .then(fac_gc_step_with(fac_ok_2))
        .then(env_consume_responses)
        .expect(gc_s.state.TIME == 1)
        .then(env_fac_fetch_reqs(party2, sid))   
        .then(fac_step_gc_wait_with(fac_fetch_1))
        .then(fac_step_gc_wait_with(fac_fetch_2))
        .then(fac_step_gc_wait_with(fac_fetch_3))
        .expect(fac_s.input.fac_get_fetch_response_messages().size() == 1)
    }


// must fail
val P1 = fac_s.input.size() == 0
val P2 = fac_s.log.size() <= 7
val P3 = fac_s.log.map(x => x.msg).fac_get_ok_response_messages().size() <= 2
val P4 = fac_s.log.map(x => x.msg).fac_get_fetch_response_messages().size() <= 2
val P5 = fac_s.log.map(x => x.msg).fac_get_send_response_messages().size() <= 5
val P6 = gc_s.input.size() == 0
val P7 = gc_s.log.map(x => x.msg).gc_get_all_response_messages().size() == 0
val P8 = gc_s.state.TIME <= 4
val P9 = fac_s.log.map(x => x.msg).fac_get_fetch_response_messages().forall(m => m.data.size() == 0)
val P10 = fac_s.SIM_handles.fac_get_sim_send_hold_messages().size() <= 2


// ∆-delayed Liveness: if P sends a message x to Q in some round t, and Q attempts to fetch in some 
// round t‘ ≥ t + ∆ then Q receives x in some round t’’ ∈ [t, t’].  
val DDL = {
    fac_s.log.forall(msg1 =>
        match msg1.msg {
            | SendRequest(p) => { 
                val fac_fetch_response_exists = fac_s.log.exists(msg3 => {
                    match msg3.msg {
                        | FetchResponse(d) => {
                            d.t_prim >= msg1.time + delta_param and p.Q == d.cfg.party
                        }
                        | _ => false
                    }
                })

                fac_fetch_response_exists implies {
                    fac_s.log.exists(msg3 => {
                        match msg3.msg {
                            | FetchResponse(d) => {
                                (p.x.in(d.data)) and p.Q == d.cfg.party
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
val Auth = fac_s.log.forall(log_entry1 => {
  match log_entry1.msg {
    | FetchResponse(p) => { // 
        // find corresponding FetchRequest in the log
        (p.data != Set()) implies fac_s.log.exists(log_entry2 => {
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


}