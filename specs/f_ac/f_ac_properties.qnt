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
    val delta_param: int = 1   


    action step : bool = any {
        fac_step,  
        gc_step_fac_wait,
        env_send_all_reqs,
        env_consume_responses,
    }

    action gc_step_fac_wait : bool =  all {
        f_ac_s' = f_ac_s,
        gc_step,        
    }

    action init = all {
        fac_init(Parties, C, delta_param, sids),
        gc_init(C,0,Set({party: 1, sid: 1}, {party:2, sid:1})),
    }

    action env_send_all_reqs : bool = all {
        nondet from = Parties.oneOf()
        nondet top = Parties.oneOf()
        nondet sid = sids.oneOf()

        any {
            env_send_reqs(from, top, sid),
            env_fetch_reqs(from, sid),
            env_ok_reqs(from, sid),
            env_fac_sim_reqs,
            env_gc_sim_reqs,
        }
    }

 
    // // Environment interaction: non-deterministically add requests when input is empty
    action env_send_reqs(from:Party, top: Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet data = Set(42,43,44,45).oneOf()
        val cfg = {party: from, sid: sid, nonce: f_ac_s.nonce}

        f_ac_s' = {...f_ac_s, 
            input: Set(SendRequest({cfg: cfg, Q: top, x: data})),
            nonce: f_ac_s.nonce + 1

        },
        gc_stutter
    }

    action env_fetch_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: f_ac_s.nonce}

        // Option 2: Fetch
        f_ac_s' = {...f_ac_s, 
            input: Set(FetchRequest({cfg: cfg})),
            nonce: f_ac_s.nonce + 1

        },
        gc_stutter   
    }

    action env_ok_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: f_ac_s.nonce}

        // Option 3: OK
        f_ac_s' = {...f_ac_s, 
            input: Set(OKRequest({cfg: cfg})),
            nonce: f_ac_s.nonce + 1

        },   
        gc_stutter     
    }

    action env_fac_sim_reqs : bool = all {
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,
        f_ac_s' = {...f_ac_s, 
            input: Set(SIMControl),
            nonce: f_ac_s.nonce + 1

        },
        gc_stutter
    }

    action env_gc_sim_reqs : bool = all {
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,
        g_clock_s' = {...g_clock_s, 
            input: Set(GC_SIMControl),
            nonce: g_clock_s.nonce + 1

        },
        f_ac_s' = f_ac_s,
    }    

    pure def env_consume_response(sys: FAC_System) : Set[FAC_Transition] = {
         sys.input.get_all_response_messages().map(msg_log =>
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
        val response_msgs = env_consume_response(f_ac_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        f_ac_s' = process_transitions(f_ac_s, transition),
        gc_stutter
    }

 
    // quint test f_ac_properties.qnt --match test1
    run test_send1 = {
        val party1 = 1
        val party2 = 2
        val sid = 1

        init.then(env_send_reqs(party1, party2, sid))
        .then(fac_step_with(send_1))
        .then(fac_step_with(send_2))
        .then(fac_step_with(send_3))
        .then(step)
        .expect(true)
    }


// must fail
val P1 = f_ac_s.input.size() == 0
val P2 = f_ac_s.log.size() <= 7
val P3 = f_ac_s.log.map(x => x.msg).get_ok_response_messages().size() <= 2
val P4 = f_ac_s.log.map(x => x.msg).get_fetch_response_messages().size() <= 2
val P5 = f_ac_s.log.map(x => x.msg).get_send_response_messages().size() <= 5
val P6 = g_clock_s.input.size() == 0
val P7 = g_clock_s.log.map(x => x.msg).gc_get_all_response_messages().size() == 0
val P8 = g_clock_s.state.TIME <= 4
val P9 = f_ac_s.log.map(x => x.msg).get_fetch_response_messages().forall(m => m.data.size() == 0)
val P10 = f_ac_s.SIM_handles.get_sim_send_hold_messages().size() <= 2


// must be true when we add counter
val SIMP = {
    f_ac_s.log.forall(msg1 => {
        match msg1.msg {
            | OKResponse(p) => {
                msg1.handles.get_sim_send_hold_messages().filter(k => k.cfg.party == p.cfg.party).size() == 0
            }
            | _ => true
        }
    })
}

// ∆-delayed Liveness: if P sends a message x to Q in some round t, and Q attempts to fetch in some 
// round t‘ ≥ t + ∆ then Q receives x in some round t’’ ∈ [t, t’].  
val DDL = {
    f_ac_s.log.forall(msg1 =>
        match msg1.msg {
            | SendRequest(p) => { 
                val fetch_response_exists = f_ac_s.log.exists(msg3 => {
                    match msg3.msg {
                        | FetchResponse(d) => {
                            d.t_prim >= msg1.time + delta_param and p.Q == d.cfg.party
                        }
                        | _ => false
                    }
                })

                fetch_response_exists implies {
                    f_ac_s.log.exists(msg3 => {
                        match msg3.msg {
                            | FetchResponse(d) => {
                                (p.x.in(d.data)) and p.Q == d.cfg.party
                                //d.t_prim >= msg1.time + delta_param and p.Q == d.cfg.party and p.x.in(d.data)
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
val Auth = f_ac_s.log.forall(log_entry1 => {
  match log_entry1.msg {
    | FetchResponse(p) => { // 
        // find corresponding FetchRequest in the log
        (p.data != Set()) implies f_ac_s.log.exists(log_entry2 => {
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


// Order preservation: fails
val Order = f_ac_s.log.forall(log_entry1 => {
  match log_entry1.msg {
    | SendResponse(p) => { // 
        // find corresponding FetchRequest in the log
        f_ac_s.log.forall(log_entry2 => {
            match log_entry2.msg {
                | SendResponse(q) => {
                        and {
                            log_entry1.idx < log_entry2.idx, // second send response happens later
                            p.Q == q.Q, // data being sent to the same receipient
                            p.x != q.x // data is different
                            } implies {
                            f_ac_s.log.forall(log_entry3 => {
                                match log_entry3.msg {
                                    | FetchResponse(d) => {
                                        q.x.in(d.data) implies {
                                            f_ac_s.log.exists(log_entry4 => {
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