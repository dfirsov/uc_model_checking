module g_ledger_properties {

    import basicSpells.* from "../../spells/basicSpells"
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"
    import g_ledger_env.* from "./g_ledger_env"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"
    import common.* from "../common"    

    import g_ledger_env from "./g_ledger_env" 


 
// must fail
val P1 = g_ledger_s.input.size() == 0
val P2 = g_ledger_s.log.size() <= 7
val P3 = g_ledger_s.log.map(x => x.msg).led_get_ok_response_messages().size() <= 2
val P4 = g_ledger_s.log.map(x => x.msg).led_get_submit_response_messages().size() <= 2
val P5 = g_ledger_s.log.map(x => x.msg).led_get_read_response_messages().size() <= 5
val P6 = g_clock_s.input.size() == 0
val P7 = g_clock_s.state.TIME <= 3
val P8 = g_ledger_s.handles.led_get_submit_sim_hold_messages().size() <= 2

// Liveness: if a tx is submitted at time t by some party, and 
// the transaction remains valid for some number of rounds, then every other party will see it as part of 
// its (local) log at time t + ∆_latency + ∆_slack.
val LED_Liveness = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | SubmitResponse(p) => {
             g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | ReadRequest(q) => {
                        g_ledger_s.log.forall(log_entry3 => {
                            match log_entry3.msg {
                                | ReadResponse(d) => {
                                    and {
                                        (log_entry1.time + delta_latency + delta_slack < log_entry2.time),
                                        log_entry1.idx < log_entry2.idx,
                                        d.cfg.nonce == q.cfg.nonce    
                                    } implies {
                                        (d.result.listHas(p.tx))
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
        | _ => true
    }
})

 
// Consistency: for any two parties P and Q, with local logs log[0:ptr[P]] 
// and log[0:ptr[Q]] we either have log[0:ptr[P]]is a prefix of log[0:ptr[Q]] or vice versa.
val LED_Consistency = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | ReadResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | ReadResponse(q) => {
                        or {
                            p.result.isPrefixOf(q.result),
                            q.result.isPrefixOf(p.result),
                        }
                    }
                    | _ => true
                }
            })
        }   
        | _ => true     
    }
})

// Validity: For any (honest) P with local log log_P, all transactions in log_P are valid. 
val LED_Validity = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | ReadRequest(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    ReadResponse(q) => {
                        (p.cfg.nonce == q.cfg.nonce) implies {
                            q.result.indices().forall(i => {
                                val tx = q.result[i]
                                // NOTE: expecting that Val is pure def
                                led_valid(log_entry1.state.Val, log_entry2.state.log.mySlice(0,i), tx).valid 
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

// Persistence: once a tx is added to log, it remains there forever.
val LED_Persistance = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | ReadResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | ReadResponse(q) => {
                        (log_entry1.idx < log_entry2.idx and p.cfg.party == q.cfg.party) implies {
                            p.result.isPrefixOf(q.result) //or q.result.isPrefixOf(p.result)
                        }
                    }
                    | _ => true
                }
            })
        }
        | _ => true
    }
})

// Agreed Prefix: TODO
val LED_AgreedPrefix = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | ReadResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | ReadRequest(q) => {
                        (q.cfg.nonce == p.cfg.nonce) implies {
                        p.result.length() >= log_entry1.state.log.indices().filter(i => {
                            log_entry2.time - log_entry1.state.log[i].time >= g_ledger_s.params.delta_slack
                        }).size()

                        }
                    }   
                    | _ => true
                }
            })

        }
        | _ => true
    }
})


val AllProps = all {
    LED_Liveness,
    LED_Consistency,
    LED_Validity,
    LED_Persistance,
    LED_AgreedPrefix,
}

temporal NeverStuck = {
  // WF is required because TLA+ semantics treat stuttering as a valid step even if not explicitly modeled
  step.weakFair(Set(g_ledger_s)).implies ({
    step.weakFair(Set(g_clock_s)).implies ({
        not (eventually(
                always(
                    or {
                        g_ledger_s.input != Set(),
                        g_clock_s.input != Set()
                    }
                )
        ))
    })
  })
}

}

