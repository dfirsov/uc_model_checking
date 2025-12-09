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
val P6 = gc_s.input.size() == 0
val P7 = gc_s.state.TIME <= 3
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

//Consistency: for any two parties P and Q, with local logs log[0:ptr[P]] 
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
val LED_Validity = g_ledger_s.log.forall(log_entry => {
    match log_entry.msg {
        | ReadRequest(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    ReadResponse(q) => {
                        (p.cfg.nonce == q.cfg.nonce) implies {
                            g_ledger_s.state.log.indices().forall(i => {
                                val tx = g_ledger_s.state.log[i].tx
                                // NOTE: expecting that Val is pure def
                                led_valid(log_entry.state.Val, log_entry.state.log, tx).valid 
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
                        (log_entry1.idx < log_entry2.idx) implies {
                            p.result.isPrefixOf(q.result)
                        }
                    }
                    | _ => true
                }
            })
        }
        | _ => true
    }
})

val LED_CommonPrefix = g_ledger_s.log.forall(log_entry => {
    match log_entry.msg {
        | ReadResponse(p) => {
            p.result.length() >= log_entry.state.log.length() - g_ledger_s.params.delta_slack
        }
        | _ => true
    }
})

}

