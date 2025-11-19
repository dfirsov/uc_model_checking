module f_diff{
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    
    import common.* from "../common"
    import f_diff_types.* from "./f_diff_types"
    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"

    var dif_s : DIF_System


    def dif_inject(sys:DIF_System) : Set[DIF_Transition] = {
        sys.input.dif_get_inject_request_messages().map(p => {
            val P = p.cfg.party
            val sid = p.cfg.sid
            if(P.in(sys.params.C)){
                val sid_state = sys.state.get(sid)
                val t = gc_s.state.TIME
                val Q = p.Q
                {
                    post_state: sys.state.set(sid, {...sid_state,
                        ctr: sid_state.ctr + 1,
                        L: sid_state.L.put({to:Q, ctr: sid_state.ctr + 1}, {data: p.m, time: t})
                    }),
                    effects: Set(DIF_UpdateInput(Set(InjectResponse(p))))
                }
            }else{
               {
                    post_state: sys.state,
                    effects: Set(
                        DIF_UpdateInput(Set())
                    )
                }
            }
        })
    }

    def dif_diffuse_1(sys: DIF_System) : Set[DIF_Transition] = { 
        sys.input.dif_get_diffuse_request_messages().map(msg => 
            val S = msg.cfg.party
            val m = msg.m
            val sid = msg.cfg.sid
            val t = gc_s.state.TIME
            val sid_state = sys.state.get(sid)

            if (and {
                S.in(dif_s.params.P),
                //sid.in(dif_s.params.SIDs), // TODO: really?
                not ((S, t).in(sid_state.D_flag)),
            }) {
                val new_D_flag = sid_state.D_flag.union(Set((S, t)))
                val new_ctr = sid_state.ctr + 1

                // computing the new_L
                // TODO: abstract this into a function for joining maps
                val new_L_vals = sys.params.P.map(party =>{
                    ({to: party, ctr: new_ctr},{data:m, time: t})
                })
                val old_L_vals = sid_state.L.keys().map(k => (k, sid_state.L.get(k)))
                val new_L = new_L_vals.union(old_L_vals).setToMap()
                

                // return the transition
                {
                    post_state : sys.state.set(sid, {...sid_state,
                        L: new_L,
                        ctr: new_ctr,
                        D_flag: new_D_flag,
                    }),
                    effects : Set(
                        DIF_LogMessage(DiffuseRequest(msg)),
                        DIF_UpdateInput(Set(SIMControl)),
                        DIF_AppendHandle(SIMDiffuseHold({ 
                            cfg: msg.cfg,
                            ctr: new_ctr,
                            t: t,
                            m: m
                        })),
                        ),
                }                  
            }else{
               {
                    post_state: sys.state,
                    effects: Set(
                        DIF_UpdateInput(Set())
                    ),
                }
            }
        )
    }    

    pure def dif_diffuse_2(sys: DIF_System) : Set[DIF_Transition] = {
        sys.input.dif_get_sim_control_messages().map(c => { // SIM must have a control
            sys.SIM_handles.dif_get_sim_diffuse_hold_messages().map(msg => {
                {
                    post_state : sys.state,
                    effects : Set(
                        DIF_UpdateInput(Set(SIMDiffuseRelease({cfg: msg.cfg, t: msg.t, ctr: msg.ctr, m:msg.m}))),
                        DIF_RemoveHandle(SIMDiffuseHold(msg)),
                    )
                }
            })
        }).flatten()
    }

    pure def dif_diffuse_3(dif: DIF_System, clock: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
        dif.input.dif_get_sim_diffuse_release_messages().map(msg => {
            val dif_transition = {
                post_state: dif.state,
                effects: Set(
                    DIF_UpdateInput(Set()),
                    DIF_AppendHandle(ClockRequestWait(msg))
                )
            }

            val clock_transition = {
                post_state: clock.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({nonce: msg.cfg.nonce, party: {party: msg.cfg.party, sid: msg.cfg.sid}})))
                )
            }

            (dif_transition, clock_transition)
        })
    }

    pure def dif_diffuse_4(dif: DIF_System, clock: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
        clock.input.get_okay_response_messages().map(msg1 => 
            dif.SIM_handles.dif_get_clock_request_wait_messages().map(msg2 => {
                if (msg1.nonce == msg2.cfg.nonce){
                    val dif_transition = {
                        post_state: dif.state,
                        effects: Set(
                            DIF_UpdateInput(Set(DiffuseResponse(msg2))),
                        )
                    }

                    val clock_transition = {
                        post_state: clock.state,
                        effects: Set(
                            GC_UpdateInput(Set())
                        )
                    }

                    Set((dif_transition, clock_transition))
                }else{
                    Set()
                }
            })
        ).flatten().flatten()
    }

    def dif_fetch_1(dif: DIF_System) : Set[DIF_Transition] = {
        dif.input.dif_get_fetch_request_messages().map(msg => 
        {
            val tp = gc_s.state.TIME // to make traces shorter
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val delta = dif.params.delta
            val sid_state = dif.state.get(sid)
            val L = sid_state.L
            
            if (and {
                P.in(dif.params.P),
                sid.in(dif.params.SIDs),
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
                    post_state : dif.state.set(sid, {...sid_state,
                        F_flag: sid_state.F_flag.union(Set((P, tp))) // adding to F_flag
                    }),
                    effects : Set(
                        DIF_LogMessage(FetchRequest(msg)),
                        DIF_UpdateInput(Set(SIMControl)),
                        DIF_AppendHandle(SIMFetchHold({
                            t_prim: tp,
                            i_late: i_late,
                            cfg: msg.cfg
                        }))
                    ),
                }
            }else{
                {
                    post_state : dif.state,
                    effects : Set(
                        DIF_UpdateInput(Set()),
                    )
                }
            }
        })
    }

    pure def dif_fetch_2(dif: DIF_System) : Set[DIF_Transition] = {
        dif.input.dif_get_sim_control_messages().map(c => { // sim must have control
            dif.SIM_handles.dif_get_sim_fetch_hold_messages().map(msg => {
                val sid = msg.cfg.sid
                val L = dif.state.get(sid).L
                dif.state.get(sid).L.keys().powerset().map(i_early => 
                //Set(Set()).map(i_early =>
                    {   
                        post_state : dif.state,
                        effects : Set( 
                            DIF_RemoveHandle(SIMFetchHold(msg)),
                            DIF_UpdateInput(Set(SIMFetchRelease({
                                cfg: msg.cfg,
                                t_prim: msg.t_prim,
                                i_late: msg.i_late,
                                i_early: i_early.map(k => k.ctr)
                            }))),
                            )  
                    }
                )
            }).flatten()
        }).flatten()
    }        

    def dif_fetch_3(dif : DIF_System) : Set[DIF_Transition] = {
        dif.input.dif_get_sim_fetch_release_messages().map(msg => 
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val t_prim = msg.t_prim
            val i_late = msg.i_late
            val state = dif.state.get(sid)
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
                post_state: dif.state.set(sid, {...state, 
                                L: new_L
                            }),
                effects: Set(
                    DIF_UpdateInput(Set(FetchResponse({cfg:msg.cfg, t_prim: msg.t_prim, data: msgs})))
                )
            }
        )
    }

   def dif_ok_1(dif : DIF_System, gc: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
        dif.input.dif_get_ok_request_messages().map(msg => 
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            val nonce = msg.cfg.nonce

            val dif_transition = {
                post_state: dif.state,
                effects: Set(
                    DIF_UpdateInput(Set()),
                    DIF_AppendHandle(OKRequestWait(msg))
                )
            }

            val gc_transition = {
                post_state: gc.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({nonce: nonce, party:{party: P, sid:sid}})))
                )
            }

            (dif_transition, gc_transition)
        })
    }     


    def dif_ok_2(dif : DIF_System, gc: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
        dif.SIM_handles.dif_get_ok_request_wait_messages().map(msg => 
        {
            gc.input.get_okay_response_messages().map(msg2 => {
                if(msg2.nonce == msg.cfg.nonce){
                    val dif_transition = {
                        post_state: dif.state,
                        effects: Set(
                            DIF_UpdateInput(Set(OKResponse(msg))),
                            DIF_RemoveHandle(OKRequestWait(msg))
                        )
                    }

                    val gc_transition = {
                        post_state: gc.state,
                        effects: Set(
                            GC_UpdateInput(Set())
                        )
                    }                    
                    Set((dif_transition, gc_transition))
                }else{
                    Set()
                }
            }).flatten()
        }).flatten()
    }       

    // sim might want to give control back to the environment
    pure def dif_sim_loses_ctrl(dif: DIF_System) : Set[DIF_Transition] = {
        dif.input.dif_get_sim_control_messages().map(c =>  // sim must have control
                    {   
                        post_state : dif.state,
                        effects : Set( 
                            DIF_UpdateInput(Set()),
                            )  
                    })
    }

    def dif_main_listener(dif: DIF_System, clock: GC_System) : (Set[(DIF_Transition, GC_Transition)]) = {
        val id : GC_Transition = {post_state: clock.state, effects: Set()}

        Set(
            dif_inject(dif),
            dif_diffuse_1(dif),
            dif_diffuse_2(dif),          
            dif_fetch_1(dif),
            dif_fetch_2(dif),
            dif_fetch_3(dif),
            dif_sim_loses_ctrl(dif)
        ).flatten().map(x => (x,id)).union(
            Set(
                dif_diffuse_3(dif, clock),
                dif_diffuse_4(dif, clock),
                dif_ok_1(dif, clock),
                dif_ok_2(dif, clock),
                ).flatten()
        )
    }           
    
}