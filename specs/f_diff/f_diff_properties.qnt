module f_diff_properties{
    

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"    
    import f_diff_types.*   from "./f_diff_types"
    import f_diff.*  from "./f_diff"
    

    val Parties = Set(1,2,3)
    val C = Set(3)
    val sids = Set(1)
    val delta_param: int = 2   


    action step : bool = any {
        dif_step,  
        gc_step_fac_wait,
        env_fac_send_all_reqs,
        env_consume_responses,
    }

    action gc_step_fac_wait : bool =  all {
        dif_s' = dif_s,
        gc_step,        
    }

    action init = all {
        dif_init(Parties, C, delta_param, sids),
        gc_init(C,0,Set({party: 1, sid: 1}, {party:2, sid:1})),
    }

    action env_fac_send_all_reqs : bool = all {
        nondet from = Parties.oneOf()
        nondet top = Parties.oneOf()
        nondet sid = sids.oneOf()

        any {
            env_dif_send_reqs(from, top, sid),
            env_dif_inject_reqs(from, top, sid),
            env_dif_fetch_reqs(from, sid),
            env_ok_reqs(from, sid),
            env_dif_sim_reqs,
            env_gc_sim_reqs,
        }
    }

 
    // // Environment interaction: non-deterministically add requests when input is empty
    action env_dif_send_reqs(from:Party, top: Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and dif_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet data = Set(42,43,44,45).oneOf()
        val cfg = {party: from, sid: sid, nonce: dif_s.nonce}
        dif_s' = {...dif_s, 
            input: Set(DiffuseRequest({cfg: cfg, m: data})),
            nonce: dif_s.nonce + 1

        },
        gc_stutter
    }

    // // Environment interaction: non-deterministically add requests when input is empty
    action env_dif_inject_reqs(from:Party, top: Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and dif_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet data = Set(42,43,44,45).oneOf()
        val cfg = {party: from, sid: sid, nonce: dif_s.nonce}
        dif_s' = {...dif_s, 
            input: Set(InjectRequest({cfg: cfg, Q: top, m: data})),
            nonce: dif_s.nonce + 1

        },
        gc_stutter
    }



    action env_dif_fetch_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and dif_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: dif_s.nonce}

        // Option 2: Fetch
        dif_s' = {...dif_s, 
            input: Set(FetchRequest({cfg: cfg})),
            nonce: dif_s.nonce + 1

        },
        gc_stutter   
    }

    action env_ok_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        gc_s.input.size() == 0 and dif_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: dif_s.nonce}

        // Option 3: OK
        dif_s' = {...dif_s, 
            input: Set(OKRequest({cfg: cfg})),
            nonce: dif_s.nonce + 1

        },   
        gc_stutter     
    }

    action env_dif_sim_reqs : bool = all {
        gc_s.input.size() == 0 and dif_s.input.size() == 0,
        dif_s' = {...dif_s, 
            input: Set(SIMControl),
            nonce: dif_s.nonce + 1
        },
        gc_stutter
    }

    action env_gc_sim_reqs : bool = all {
        gc_s.input.size() == 0 and dif_s.input.size() == 0,
        gc_s' = {...gc_s, 
            input: Set(GC_SIMControl),
            nonce: gc_s.nonce + 1
        },
        dif_s' = dif_s,
    }

    pure def env_consume_response(sys: DIF_System) : Set[DIF_Transition] = {
         sys.input.dif_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                DIF_LogMessage(msg_log),
                DIF_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(dif_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        dif_s' = dif_process_transitions(dif_s, transition),
        gc_stutter
    }


// ∆-delayed Liveness: if P sends a message x to Q in some round t, and Q attempts to fetch in some 
// round t‘ ≥ t + ∆ then Q receives x in some round t’’ ∈ [t, t’].  
val DDL = {
    dif_s.log.forall(msg1 =>
        match msg1.msg {
            | DiffuseRequest(p) => { 
                val dif_fetch_response_exists = dif_s.log.exists(msg3 => {
                    match msg3.msg {
                        | FetchResponse(d) => {
                            d.t_prim >= msg1.time + delta_param  //and p.Q == d.cfg.party
                        }
                        | _ => false
                    }
                })

                dif_fetch_response_exists implies {
                    dif_s.log.exists(msg3 => {
                        match msg3.msg {
                            | FetchResponse(d) => {
                                (p.m.in(d.data)) //and p.Q == d.cfg.party
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

// Authentication: If a nonempty set of messages was delivered from P to Q then P indeed sent a diffuse message in the past.
val Auth = dif_s.log.forall(log_entry1 => {
  match log_entry1.msg {
    | FetchResponse(p) => { // 
        // find corresponding FetchRequest in the log
        (p.data != Set()) implies dif_s.log.exists(log_entry2 => {
          match log_entry2.msg {
            | DiffuseRequest(q) => {
                 q.m.in(p.data)
              }
            | _ => false
          }
        })
      }
    | _ => true
  }
})    

}