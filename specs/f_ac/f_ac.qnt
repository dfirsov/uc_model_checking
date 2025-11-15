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
                sid.in(f_ac_s.params.SIDs)
            }) {
                val new_ctr = sid_state.ctr + 1
                val new_done = sid_state.done.put({from: P, to: Q, time: t}, true)
                val new_L = sid_state.L.put(new_ctr, {from: P, to: Q, time: t, data: x})

                Set(
                    {
                        post_state : sys.state.set(sid, {
                            L: new_L,
                            ctr: new_ctr,
                            done: new_done
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
                )
            }else{
                Set()
            }
        ).flatten()
    }

    pure def send_2(sys: FAC_System) : Set[FAC_Transition] = {
        sys.input.get_sim_control_messages().map(c => { // SIM must have a control
            sys.SIM_handles.get_sim_send_hold_messages().map(msg => {
                {
                    post_state : sys.state,
                    effects : Set(
                        FAC_UpdateInput(Set(SIMSendRelease({cfg: msg.cfg, Q: msg.Q, t: msg.t, x:msg.x}))),
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
                    post_state : sys.state.set(sid, {...sid_state, 
                        done: sid_state.done.put({from: msg.cfg.party, to: msg.Q, time: msg.t}, false)
                    }),
                    effects : Set(
                        FAC_UpdateInput(Set(SendResponse({cfg: msg.cfg, Q: msg.Q, t: msg.t, x: msg.x}))),
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
            val L = sys.state.get(sid).L
            
            if (and {
                P.in(f_ac_s.params.P),
                sid.in(f_ac_s.params.SIDs)
            }){
                val i_late = L.keys().filterMap(k => {
                    val v = L.get(k)
                    if(v.to == P and (tp - v.time) >= delta){
                        Some(k)
                    }else{
                        None
                    }
                })

                Set({
                    post_state : sys.state,
                    effects : Set(
                        FAC_LogMessage(FetchRequest(msg)),
                        FAC_UpdateInput(Set(SIMControl)),
                        FAC_AppendHandle(SIMFetchHold({
                            t_prim: tp,
                            i_late: i_late,
                            cfg: msg.cfg
                        }))
                    ),
                })
            }else{
                Set()
            }
        }).flatten()
    }

    pure def fetch_2(sys: FAC_System) : Set[FAC_Transition] = {
        sys.input.get_sim_control_messages().map(c => { // sim must have control
            sys.SIM_handles.get_sim_fetch_hold_messages().map(msg => {
                val sid = msg.cfg.sid
                val L = sys.state.get(sid).L
                L.keys().powerset().map(i_early => 
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
            val t_prim = msg.t_prim
            val i_late = msg.i_late
            val state = sys.state.get(sid)
            val L = state.L
            val i_early = msg.i_early.filterMap({i =>
                val v = L.get(i)
                if(v.to == msg.cfg.party){
                    Some(i)
                }else{
                    None
                }
            })

            val msgs = i_early.union(i_late).map(k => L.get(k).data)
            val new_L = L.keys().exclude(i_early.union(i_late)).mapBy(k => L.get(k))

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

    def ok_1(sys: FAC_System) : Set[FAC_Transition] = {
        sys.input.get_ok_request_messages().map(msg => {
            val t = g_clock_s.state.TIME // to make traces shorter
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val done = sys.state.get(sid).done
            val flag = sys.params.P.forall(Q => {
                val key = {from:P, to:Q, time: t}
                done.has(key) implies done.get(key)
            })

            if(sid.in(sys.params.SIDs) and P.in(sys.params.P) and flag){
                Set({
                    post_state: sys.state,
                    effects: Set(
                        FAC_UpdateInput(Set(ClockRequest({cfg: msg.cfg, t: t}))),
                        FAC_LogMessage(OKRequest(msg))
                    )
                })
            }else{
                Set()
            }
        }).flatten()
    }

    def ok_2(fac:FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        fac.input.get_clock_request_messages().map(msg => 
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            val nonce = msg.cfg.nonce

            val fac_transition = {
                post_state: fac.state,
                effects: Set(
                    FAC_UpdateInput(Set()),
                    FAC_AppendHandle(ClockRequestWait({cfg:msg.cfg, t: msg.t}))
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

    def ok_3(fac:FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        fac.SIM_handles.get_clock_request_wait_messages().map(msg => 
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            
            clock.input.get_okay_response_messages().map(msg2 => {
                if(msg2.nonce == msg.cfg.nonce){
                    val fac_transition = {
                        post_state: fac.state,
                        effects: Set(
                            FAC_UpdateInput(Set(ClockResponse({cfg:msg.cfg, t: msg.t}))),
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


    def ok_4(sys : FAC_System) : Set[FAC_Transition] = {
        sys.input.get_clock_response_messages().map(msg => {
            val P = msg.cfg.party
            val t = msg.t
            val state = sys.state.get(msg.cfg.sid)
            val done = state.done.keys().exclude(
              state.done.keys().filterMap(k => 
                {
                  if(k.from == P and k.time == t){
                    None
                  }else{
                    Some(k)
                  }
                }
              )
            ).mapBy(k => state.done.get(k))
            
            {
                post_state: sys.state.set(msg.cfg.sid, {...state, 
                    done: done
                }),
                effects: Set(
                    FAC_UpdateInput(Set(OKResponse({cfg: msg.cfg}))),
                )
            }

        })
    }

    def main_listener(sys: FAC_System, clock: GC_System) : (Set[(FAC_Transition, GC_Transition)]) = {
        val id : GC_Transition = {post_state: clock.state, effects: Set()}
        Set(
            send_1(sys),
            send_2(sys),
            send_3(sys),
            fetch_1(sys),
            fetch_2(sys),
            fetch_3(sys),
            ok_1(sys),
            ok_4(sys)
        ).flatten().map(x => (x,id)).union(
            Set(
                ok_2(sys, clock),
                ok_3(sys, clock)
                ).flatten()
        )
    }

    pure def process_transitions(sys: FAC_System, transition: FAC_Transition) : FAC_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => apply_effect(a1,a2)))
    } 

    action fac_step : bool =  any {
        all{ 
            val q = main_listener(f_ac_s, g_clock_s)
            all {
                q.size() > 0,
                nondet transition = q.oneOf()
                all {
                    f_ac_s' = process_transitions(f_ac_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        },
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
                    done : Map()
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

}