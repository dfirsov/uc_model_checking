module f_ac_properties {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"    
    import f_ac_types.*   from "./f_ac_types"
    import f_ac.*  from "./f_ac"
    import f_ac_env.*  from "./f_ac_env"
    
 

// ========== CO-PROPERTIES (should fail - used for model checking bounds) ==========

val P1 = f_ac_s.input.size() == 0
val P2 = f_ac_s.log.size() <= 7
val P3 = f_ac_s.log.map(x => x.msg).fac_get_ok_response_messages().size() <= 2
val P4 = f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().size() <= 2
val P5 = f_ac_s.log.map(x => x.msg).fac_get_send_response_messages().size() <= 5
val P6 = g_clock_s.input.size() == 0
val P7 = g_clock_s.log.map(x => x.msg).gc_get_all_response_messages().size() == 0
val P8 = g_clock_s.state.TIME <= 4
val P9 = f_ac_s.log.map(x => x.msg).fac_get_fetch_response_messages().forall(m => m.data.size() == 0)
val P10 = f_ac_s.handles.fac_get_sim_send_hold_messages().size() <= 2

// Fetched messages must be at least delta old
val DeltaDelay = f_ac_s.log.forall(log_entry => {
    match log_entry.msg {
        | FetchResponse(p) => {
            p.data.forall(data => {
                // Find the send of this data
                f_ac_s.log.exists(log_entry2 => {
                    match log_entry2.msg {
                        | SendRequest(q) => {
                            and {
                                q.x == data,
                                q.Q == p.cfg.party,
                                log_entry.time >= log_entry2.time + delta_param
                            }
                        }
                        | _ => false
                    }
                })
            })
        }
        | _ => true
    }
})

// At time t a message is delivered at most once
val NoDuplication = f_ac_s.log.forall(log1 => {
    match log1.msg {
        | FetchResponse(fr1) => {
            f_ac_s.log.forall(log2 => {
                match log2.msg {
                    | FetchResponse(fr2) => {
                        and {
                            log1.idx != log2.idx,
                            fr1.cfg.party == fr2.cfg.party,
                            fr1.cfg.sid == fr2.cfg.sid,
                            log1.time == log2.time
                        } implies {
                            // Data sets should be disjoint (no message delivered twice)
                            fr1.data.intersect(fr2.data).size() == 0
                        }
                    }
                    | _ => true
                }
            })
        }
        | _ => true
    }
})

// Co-property: Message sent at time t cannot be fetched before t + delta (should fail - messages can be fetched early)
val AtLeastDeltaDelayed = and {
    f_ac_s.log.map(x => x.msg).fac_get_fetch_request_messages().size() == 1,
    f_ac_s.log.map(x => x.msg).fac_get_send_request_messages().size() == 1
} implies
    
    f_ac_s.log.forall(log_entry => {
        match log_entry.msg {
            | FetchResponse(fr) => {
                // For each data item in the fetch response
                fr.data.forall(data => {
                    // Find when this data was sent
                    f_ac_s.log.forall(log_entry2 => {
                        match log_entry2.msg {
                            | SendResponse(sr) => {
                                // If this is the matching send
                                (sr.x == data and sr.Q == fr.cfg.party) implies {
                                    // Then fetch time must be at least t + delta
                                    log_entry.time >= log_entry2.time + delta_param
                                }
                            }
                            | _ => true
                        }
                    })
                })
            }
            | _ => true
        }
    })

// ========== PROPERTIES ==========
// ∆-delayed Liveness: if P sends a message x to Q in some round t, and Q attempts to fetch in some 
// round t‘ ≥ t + ∆ then Q receives x in some round t’’ ∈ [t, t’].  
val DDL = {
    f_ac_s.log.forall(msg1 =>
        match msg1.msg {
            | SendRequest(p) => { 
                val fac_fetch_response_exists = f_ac_s.log.exists(msg3 => {
                    match msg3.msg {
                        | FetchResponse(d) => {
                            d.t_prim >= msg1.time + delta_param and p.Q == d.cfg.party
                        }
                        | _ => false
                    }
                })

                fac_fetch_response_exists implies {
                    f_ac_s.log.exists(msg3 => {
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


// ========== BASIC SYSTEM PROPERTIES ==========

// Handles are properly managed (no orphaned holds)
val NoOrphanedHolds = f_ac_s.handles.forall(h => {
    match h {
        | SIMSendHold(_) => true
        | SIMFetchHold(_) => true
        | ClockRequestWait(_) => true
        | OKRequestWait(_) => true
        | _ => false
    }
})

// Input buffer has at most one message (as per design)
val InputBound = f_ac_s.input.size() <= 1

// Counter monotonically increases
val CounterMonotonic = sids.forall(sid => {
    f_ac_s.state.get(sid).ctr >= 0
})

// Messages in L have valid counters
val ValidCounters = sids.forall(sid => {
    val sid_state = f_ac_s.state.get(sid)
    sid_state.L.keys().forall(k => {
        k.ctr > 0 and k.ctr <= sid_state.ctr
    })
})

// ========== FLAG PROPERTIES ==========

// S_flag correctness: party can send at most once per time slot
val SFlagUnique = sids.forall(sid => {
    val sid_state = f_ac_s.state.get(sid)
    sid_state.S_flag.forall(entry1 => {
        sid_state.S_flag.forall(entry2 => {
            (entry1._1 == entry2._1 and entry1._2 == entry2._2) implies (entry1 == entry2)
        })
    })
})

// F_flag correctness: party can fetch at most once per time slot
val FFlagUnique = sids.forall(sid => {
    val sid_state = f_ac_s.state.get(sid)
    sid_state.F_flag.forall(entry1 => {
        sid_state.F_flag.forall(entry2 => {
            (entry1._1 == entry2._1 and entry1._2 == entry2._2) implies (entry1 == entry2)
        })
    })
})

// S_flag and F_flag correspond to actual operations in log
val FlagsMatchLog = sids.forall(sid => {
    val sid_state = f_ac_s.state.get(sid)
    and {
        // Every S_flag entry has a corresponding SendRequest
        sid_state.S_flag.forall(flag => {
            f_ac_s.log.exists(log_entry => {
                match log_entry.msg {
                    | SendRequest(p) => {
                        p.cfg.party == flag._1 and log_entry.time == flag._2
                    }
                    | _ => false
                }
            })
        }),
        // Every F_flag entry has a corresponding FetchRequest
        sid_state.F_flag.forall(flag => {
            f_ac_s.log.exists(log_entry => {
                match log_entry.msg {
                    | FetchRequest(p) => {
                        p.cfg.party == flag._1 and log_entry.time == flag._2
                    }
                    | _ => false
                }
            })
        })
    }
})

// ========== ORDERING PROPERTIES ==========

// Send must precede SendResponse
val SendBeforeResponse = f_ac_s.log.forall(log_entry => {
    match log_entry.msg {
        | SendResponse(p) => {
            f_ac_s.log.exists(log_entry2 => {
                match log_entry2.msg {
                    | SendRequest(q) => {
                        and {
                            log_entry2.idx < log_entry.idx,
                            q.cfg.party == p.cfg.party,
                            q.Q == p.Q,
                            q.x == p.x
                        }
                    }
                    | _ => false
                }
            })
        }
        | _ => true
    }
})

// Fetch must precede FetchResponse
val FetchBeforeResponse = f_ac_s.log.forall(log_entry => {
    match log_entry.msg {
        | FetchResponse(p) => {
            f_ac_s.log.exists(log_entry2 => {
                match log_entry2.msg {
                    | FetchRequest(q) => {
                        and {
                            log_entry2.idx < log_entry.idx,
                            q.cfg.party == p.cfg.party,
                            q.cfg.sid == p.cfg.sid
                        }
                    }
                    | _ => false
                }
            })
        }
        | _ => true
    }
})

// ========== MESSAGE INTEGRITY PROPERTIES ==========

// No duplicate messages with same counter
val UniqueCounters = sids.forall(sid => {
    val sid_state = f_ac_s.state.get(sid)
    sid_state.L.keys().forall(k1 => {
        sid_state.L.keys().forall(k2 => {
            (k1.ctr == k2.ctr) implies (k1 == k2)
        })
    })
})

// Messages in L have consistent timestamps (bounded by current time)
val ValidTimestamps = sids.forall(sid => {
    val sid_state = f_ac_s.state.get(sid)
    sid_state.L.keys().forall(k => {
        val msg_data = sid_state.L.get(k)
        msg_data.time <= g_clock_s.state.TIME
    })
})





// ========== ACCESS CONTROL PROPERTIES ==========

// Only authorized parties can send
val SendAccessControl = f_ac_s.log.forall(log_entry => {
    match log_entry.msg {
        | SendRequest(p) => {
            and {
                p.cfg.party.in(f_ac_s.params.P),
                p.cfg.sid.in(f_ac_s.params.SIDs),
                p.Q.in(f_ac_s.params.P)
            }
        }
        | _ => true
    }
})

// Only authorized parties can fetch
val FetchAccessControl = f_ac_s.log.forall(log_entry => {
    match log_entry.msg {
        | FetchRequest(p) => {
            and {
                p.cfg.party.in(f_ac_s.params.P),
                p.cfg.sid.in(f_ac_s.params.SIDs)
            }
        }
        | _ => true
    }
})

// ========== COMBINED PROPERTIES ==========

val AllBasicProps = and {
    NoOrphanedHolds,
    InputBound,
    CounterMonotonic,
    ValidCounters,
}

val AllFlagProps = and {
    SFlagUnique,
    FFlagUnique,
    FlagsMatchLog,
}

val AllOrderingProps = and {
    SendBeforeResponse,
    FetchBeforeResponse,
}

val AllIntegrityProps = and {
     UniqueCounters,
     ValidTimestamps,
}

val AllAccessProps = and {
    SendAccessControl,
    FetchAccessControl,
}

val AllSecurityProps = and {
    Auth,
    DDL,
}

val AllProps = and {
    AllBasicProps,
    AllFlagProps,
    AllOrderingProps,
    AllIntegrityProps,
    AllAccessProps,
    AllSecurityProps,
}

}