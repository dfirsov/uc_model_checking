module f_ac{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    
    import common.* from "../common"
    import f_ac_types.* from "./f_ac_types"
    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"


    // mutable variable represents the system state
    var f_ac_s : FAC_System


    def send_1(sys: FAC_System) :  Set[FAC_Transition] = { 
        sys.input.get_send_request_messages().map(msg => 
            val Q = msg.Q
            val x = msg.x
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val t = g_clock_s.state.TIME
            val sid_state = sys.state.get(sid)

            if (and {
                Q.in(f_ac_s.params.P),
                P.in(f_ac_s.params.P),
                sid.in(f_ac_s.params.SIDs),
                not ((P, t).in(sid_state.S_flag)),
            }) {
                val new_ctr = sid_state.ctr + 1

                val new_L = sid_state.L.put({to: Q, ctr: new_ctr}, {from: Q, time: t, data: x})
                val new_S_flag = sid_state.S_flag.union(Set((P, t)))

                {
                    post_state : sys.state.set(sid, {
                        L: new_L, //{ if (q == 1) { new_L } else { new_L } },
                        ctr: new_ctr,
                        S_flag: new_S_flag,
                        F_flag: sid_state.F_flag
                    }),
                    effects : Set(
                        FAC_LogMessage(SendRequest(msg)),
                        FAC_UpdateInput(Set(SIMControl)),
                        FAC_AppendHandle(SIMSendHold({
                            cfg: msg.cfg,
                            ctr: new_ctr,
                            Q: Q,
                            t: t,
                            x: x
                        })),
                    )
                }                    
                
            }else{
                {
                    post_state: sys.state,
                    effects: Set(
                        FAC_UpdateInput(Set())
                    ),
                }
            }
        )
    }

    pure def send_2(sys: FAC_System) : Set[FAC_Transition] = {
        sys.input.get_sim_control_messages().map(c => { // SIM must have a control
            sys.SIM_handles.get_sim_send_hold_messages().map(msg => {
                {
                    post_state : sys.state,
                    effects : Set(
                        FAC_UpdateInput(Set(SIMSendRelease({cfg: msg.cfg, Q: msg.Q, t: msg.t, ctr: msg.ctr, x:msg.x}))),
                        FAC_RemoveHandle(SIMSendHold(msg)),
                    )
                }
            })
        }).flatten()

    }    

    pure def send_3(sys: FAC_System) : Set[FAC_Transition] = {
        sys.input.get_sim_send_release_messages().map(msg => {
            val sid = msg.cfg.sid
            val sid_state = sys.state.get(sid)
                {
                    post_state : sys.state,
                    effects : Set(
                        FAC_UpdateInput(Set(ClockRequest(msg))),
                    )
                }                                  
        })
    }
    

    def send_4(fac:FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        fac.input.get_clock_request_messages().map(msg => 
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            val nonce = msg.cfg.nonce

            val fac_transition = {
                post_state: fac.state,
                effects: Set(
                    FAC_UpdateInput(Set()),
                    FAC_AppendHandle(ClockRequestWait(msg))
                )
            }


            val clock_transition = {
                post_state: clock.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({nonce: nonce, party:{party: P, sid:sid}})))
                )
            }

            Set((fac_transition, clock_transition))
        }).flatten()
    }

    def send_5(fac:FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        fac.SIM_handles.get_clock_request_wait_messages().map(msg => 
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            
            clock.input.get_okay_response_messages().map(msg2 => {
                if(msg2.nonce == msg.cfg.nonce){
                    val fac_transition = {
                        post_state: fac.state,
                        effects: Set(
                            FAC_UpdateInput(Set(ClockResponse(msg))),
                            FAC_RemoveHandle(ClockRequestWait(msg))
                        )
                    }

                    val clock_transition = {
                        post_state: clock.state,
                        effects: Set(
                            GC_UpdateInput(Set())
                        )
                    }                    
                    Set((fac_transition, clock_transition))
                }else{
                    Set()
                }
            }).flatten()
        }).flatten()
    }        

    def send_6(sys : FAC_System) : Set[FAC_Transition] = {
        sys.input.get_clock_response_messages().map(msg => {
            val P = msg.cfg.party
            val t = msg.t
            val state = sys.state.get(msg.cfg.sid)
            {
                post_state: sys.state,
                effects: Set(
                    FAC_UpdateInput(Set(SendResponse(msg))),
                )
            }

        })
    }



    def fetch_1(sys: FAC_System) : Set[FAC_Transition] = {
        sys.input.get_fetch_request_messages().map(msg => 
        {
            val tp = g_clock_s.state.TIME // to make traces shorter
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val delta = sys.params.delta
            val sid_state = sys.state.get(sid)
            val L = sid_state.L
            
            if (and {
                P.in(f_ac_s.params.P),
                sid.in(f_ac_s.params.SIDs),
                not ((P, tp).in(sid_state.F_flag)),
            }){
                val i_late = L.keys().filterMap(k => {
                    val v = L.get(k)
                    if(k.to == P and (tp - v.time) >= delta){
                        Some(k.ctr)
                    }else{
                        None
                    }
                })

               {
                    post_state : sys.state.set(sid, {...sid_state,
                        F_flag: sid_state.F_flag.union(Set((P, tp))) // adding to F_flag
                    }),
                    effects : Set(
                        FAC_LogMessage(FetchRequest(msg)),
                        FAC_UpdateInput(Set(SIMControl)),
                        FAC_AppendHandle(SIMFetchHold({
                            t_prim: tp,
                            i_late: i_late,
                            cfg: msg.cfg
                        }))
                    ),
                }
            }else{
                {
                    post_state : sys.state,
                    effects : Set(
                        FAC_UpdateInput(Set()),
                    )
                }
            }
        })
    }

    pure def fetch_2(sys: FAC_System) : Set[FAC_Transition] = {
        sys.input.get_sim_control_messages().map(c => { // sim must have control
            sys.SIM_handles.get_sim_fetch_hold_messages().map(msg => {
                val sid = msg.cfg.sid
                val L = sys.state.get(sid).L
                //L.keys().powerset().map(i_early => 
                Set(Set()).map(i_early =>
                    {   
                        post_state : sys.state,
                        effects : Set( 
                            FAC_RemoveHandle(SIMFetchHold(msg)),
                            FAC_UpdateInput(Set(SIMFetchRelease({
                                cfg: msg.cfg,
                                t_prim: msg.t_prim,
                                i_late: msg.i_late,
                                i_early: i_early
                            }))),
                            )  
                    }
                )
            }).flatten()
        }).flatten()
    }    


    def fetch_3(sys : FAC_System) : Set[FAC_Transition] = {
        sys.input.get_sim_fetch_release_messages().map(msg => 
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val t_prim = msg.t_prim
            val i_late = msg.i_late
            val state = sys.state.get(sid)
            val L = state.L
            val i_early = msg.i_early.filterMap({i =>
                if(L.has({to: P, ctr: i})){
                    Some(i)
                }else{
                    None
                }
            })

            val msgs = i_early.union(i_late).filter(k => L.has({to: P, ctr: k})).map(k => L.get({to: P, ctr: k}).data) // reported broken indexing
            val new_L = L.keys().exclude(i_early.union(i_late).map(k => {to: P, ctr: k})).mapBy(k => L.get(k))

            {
                post_state: sys.state.set(sid, {...state, 
                                L: new_L
                            }),
                effects: Set(
                    FAC_UpdateInput(Set(FetchResponse({cfg:msg.cfg, t_prim: msg.t_prim, data: msgs})))
                )
            }
        )
    }

    def ok_1(fac : FAC_System, gc: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        fac.input.get_ok_request_messages().map(msg => 
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            val nonce = msg.cfg.nonce

            val fac_transition = {
                post_state: fac.state,
                effects: Set(
                    FAC_UpdateInput(Set()),
                    FAC_AppendHandle(OKRequestWait(msg))
                )
            }

            val gc_transition = {
                post_state: gc.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({nonce: nonce, party:{party: P, sid:sid}})))
                )
            }

            (fac_transition, gc_transition)
        })
    }
    
    def ok_2(fac : FAC_System, gc: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        fac.SIM_handles.get_ok_request_wait_messages().map(msg => 
        {
            gc.input.get_okay_response_messages().map(msg2 => {
                if(msg2.nonce == msg.cfg.nonce){
                    val fac_transition = {
                        post_state: fac.state,
                        effects: Set(
                            FAC_UpdateInput(Set(OKResponse(msg))),
                            FAC_RemoveHandle(OKRequestWait(msg))
                        )
                    }

                    val gc_transition = {
                        post_state: gc.state,
                        effects: Set(
                            GC_UpdateInput(Set())
                        )
                    }                    
                    Set((fac_transition, gc_transition))
                }else{
                    Set()
                }
            }).flatten()
        }).flatten()
    }


    // sim might want to give control back to the environment
    pure def fac_sim_loses_ctrl(sys: FAC_System) : Set[FAC_Transition] = {
        sys.input.get_sim_control_messages().map(c =>  // sim must have control
                    {   
                        post_state : sys.state,
                        effects : Set( 
                            FAC_UpdateInput(Set()),
                            )  
                    })
    }    


    def main_listener(sys: FAC_System, clock: GC_System) : (Set[(FAC_Transition, GC_Transition)]) = {
        val id : GC_Transition = {post_state: clock.state, effects: Set()}

        Set(
            send_1(sys),
            send_2(sys),
            send_3(sys),
            send_6(sys),            
            fetch_1(sys),
            fetch_2(sys),
            fetch_3(sys),
            fac_sim_loses_ctrl(sys)
        ).flatten().map(x => (x,id)).union(
            Set(
                send_4(sys, clock),
                send_5(sys, clock),
                ok_1(sys, clock),
                ok_2(sys, clock),
                ).flatten()
        )
    }

    def fac_process_transitions(sys: FAC_System, transition: FAC_Transition) : FAC_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => apply_effect(a1,a2)))
    } 

    action fac_step : bool =  all {
        fac_gc_step_with(main_listener)
    }

    action fac_gc_step_with(q: (FAC_System, GC_System) => Set[(FAC_Transition, GC_Transition)]): bool = {
        all{ 
            val ts = q(f_ac_s, g_clock_s)
            all {
                ts.size() > 0,
                nondet transition = ts.oneOf()
                all {
                    f_ac_s' = fac_process_transitions(f_ac_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }    

    action fac_wait_gc_step_with(q: (GC_System) => Set[(GC_Transition)]): bool = {
        def qq = (fac1, gc1) => q(gc1).map(t => ({post_state: fac1.state, effects: Set()}, t)) 
        fac_gc_step_with(qq)

    }

    action fac_step_gc_wait_with(q: (FAC_System) => Set[(FAC_Transition)]): bool = {
        def qq = (fac1, gc1) => q(fac1).map(t => (t, {post_state: gc1.state, effects: Set()})) 
        fac_gc_step_with(qq)
    }
    



    pure def initial_f_ac(P: Set[Party], C: Set[Party], delta: Time, SIDs: Set[SID]) : FAC_System = {
        {
            params : {
                P : P,
                C : C,
                delta: delta,
                SIDs: SIDs,
            },
            state : SIDs.mapBy(sid => {
                {
                    L : Map(),
                    ctr : 0,
                    F_flag : Set(),
                    S_flag : Set()
                }
            }),
            input : Set(),
            nonce : 0,        // to be able to match requests and responses in the trace
            log : Set(),
            SIM_handles : Set()
        }        
    }

    action fac_init(P: Set[Party], C: Set[Party], delta: Time, SIDs: Set[SID]) : bool =  all {
        f_ac_s' = initial_f_ac(P, C, delta, SIDs)
    }

    def apply_effect(sys: FAC_System, eff: FAC_Effect): FAC_System = {
        match eff {
        | FAC_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg,  time: g_clock_s.state.TIME, handles: sys.SIM_handles})
            }
        | FAC_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | FAC_AppendHandle(msg) => 
            {...sys,
                SIM_handles: sys.SIM_handles.union(Set(msg)),
            }
        | FAC_RemoveHandle(handle_to_remove) =>
            {...sys,
                SIM_handles: sys.SIM_handles.exclude(Set(handle_to_remove))
            }
        }
    }    

}