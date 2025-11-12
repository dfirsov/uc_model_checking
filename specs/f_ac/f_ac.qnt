module f_ac{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import uc.* from "../uc"
    import f_ac_types.* from "./f_ac_types"
    import g_clock as g_clock from "../g_clock/g_clock"
    import g_clock_types as gct from "../g_clock/g_clock_types"


    // mutable variable represents the system state
    var f_ac : System


    def send_1(sys: System) :  Set[Transition] = { 
        sys.input.get_send_request_messages().map(msg => 
            val Q = msg.Q
            val x = msg.x
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val t = g_clock::s.state.TIME
            val sid_state = sys.state.get(sid)

            if (and {
                Q.in(f_ac.params.P),
                P.in(f_ac.params.P),
                sid.in(f_ac.params.SIDs)
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
                            LogMessage(SendRequest(msg)),
                            UpdateInput(Set(SIMControl)),
                            AppendHandle(SIMSendHold({
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

    pure def send_2(sys: System) : Set[Transition] = {
        sys.SIM_handles.get_sim_send_hold_messages().map(msg => {
            {
                post_state : sys.state,
                effects : Set(
                    UpdateInput(Set(SIMSendRelease({cfg: msg.cfg, Q: msg.Q, t: msg.t}))),
                    RemoveHandle(SIMSendHold(msg))
                )
            }
        })
    }    

    pure def send_3(sys: System) : Set[Transition] = {
        sys.input.get_sim_send_release_messages().map(msg => {
            val sid = msg.cfg.sid
            val sid_state = sys.state.get(sid)

            Set(
                {
                    post_state : sys.state.set(sid, {...sid_state, 
                        done: sid_state.done.put({from: msg.cfg.party, to: msg.Q, time: msg.t}, false)
                    }),
                    effects : Set(
                        UpdateInput(Set(SendResponse({cfg: msg.cfg}))),
                    )
                }                    
            )            
        }).flatten()
    }

    def fetch_1(sys: System) : Set[Transition] = {
        sys.input.get_fetch_request_messages().map(msg => 
        {
            val tp = g_clock::s.state.TIME // to make traces shorter
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val delta = sys.params.delta
            val L = sys.state.get(sid).L
            
            if (and {
                P.in(f_ac.params.P),
                sid.in(f_ac.params.SIDs)
            }){
                val i_late = L.keys().filterMap(k => {
                    val v = L.get(k)
                    if(v.to == P and (tp - v.time) == delta){
                        Some(k)
                    }else{
                        None
                    }
                })

                Set({
                    post_state : sys.state,
                    effects : Set(
                        LogMessage(FetchRequest(msg)),
                        UpdateInput(Set(SIMControl)),
                        AppendHandle(SIMFetchHold({
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

    pure def fetch_2(sys: System) : Set[Transition] = {
        sys.SIM_handles.get_sim_fetch_hold_messages().map(msg => {
            val sid = msg.cfg.sid
            val L = sys.state.get(sid).L
            L.keys().powerset().map(i_early => 
                {   
                    post_state : sys.state,
                    effects : Set( 
                        RemoveHandle(SIMFetchHold(msg)),
                        UpdateInput(Set(SIMFetchRelease({
                            cfg: msg.cfg,
                            t_prim: msg.t_prim,
                            i_late: msg.i_late,
                            i_early: i_early
                        }))),
                        )  
                }
            )
        }).flatten()
    }    

    def fetch_3(sys : System) : Set[Transition] = {
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

        Set({
            post_state: sys.state.set(sid, {...state, 
                        L: new_L
                    }),
            effects: Set(
                UpdateInput(Set(FetchResponse({cfg:msg.cfg, data: msgs})))
            )
        })

        ).flatten()
    }

    def ok_1(sys: System) : Set[Transition] = {
        sys.input.get_ok_request_messages().map(msg => {
            val t = g_clock::s.state.TIME // to make traces shorter
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
                        UpdateInput(Set(ClockRequest({cfg: msg.cfg, t: t}))),
                        LogMessage(OKRequest(msg))
                    )
                })
            }else{
                Set()
            }
        }).flatten()
    }

    def ok_2(fac:System, clock: gct::System) : Set[(Transition, gct::Transition)] = {
        fac.input.get_clock_request_messages().map(msg => 
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            val nonce = msg.cfg.nonce

            val fac_transition = {
                post_state: fac.state,
                effects: Set(
                    AppendHandle(ClockRequestWait({cfg:msg.cfg, t: msg.t}))
                )
            }

            val clock_transition = {
                post_state: clock.state,
                effects: Set(
                    gct::UpdateInput(Set(gct::OkayRequest({nonce: nonce, party:{party: P, sid:sid}})))
                )
            }

            Set((fac_transition, clock_transition))
        }).flatten()
    }

    def ok_3(fac:System, clock: gct::System) : Set[(Transition, gct::Transition)] = {
        fac.SIM_handles.get_clock_request_wait_messages().map(msg => 
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            
            clock.input.gct::get_okay_response_messages().map(msg2 => {
                if(msg2.nonce == msg.cfg.nonce){
                    val fac_transition = {
                        post_state: fac.state,
                        effects: Set(
                            UpdateInput(Set(ClockResponse({cfg:msg.cfg, t: msg.t}))),
                            RemoveHandle(ClockRequestWait(msg))
                        )
                    }

                    val clock_transition = {
                        post_state: clock.state,
                        effects: Set(
                            gct::UpdateInput(Set())
                        )
                    }                    
                    Set((fac_transition, clock_transition))
                }else{
                    Set()
                }
            }).flatten()
        }).flatten()
    }    

    def ok_4(sys : System) : Set[Transition] = {
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
            
            Set({
                post_state: sys.state.set(msg.cfg.sid, {...state, 
                    done: done
                }),
                effects: Set(
                    UpdateInput(Set(OKResponse({cfg: msg.cfg}))),
                )
            })

        }).flatten()
    }

    def main_listener(sys: System, clock: gct::System) : (Set[(Transition, gct::Transition)]) = {
        val id : gct::Transition = {post_state: clock.state, effects: Set()}
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


    pure def process_transitions(sys: System, transition: Transition) : System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => apply_effect(a1,a2)))
    } 

    action step : bool =  any {
        all{ 
            val q = main_listener(f_ac, g_clock::s)
            all {
                q.size() > 0,
                nondet transition = q.oneOf()
                all {
                    f_ac' = process_transitions(f_ac, transition._1),
                    g_clock::s' = g_clock::process_transitions(g_clock::s, transition._2)
                }
            }
        },
    }    

    pure def initial_f_ac() : System = {
    {
            params : {
                P : Set(),
                C : Set(),
                delta: 1,
                SIDs: Set(),
            },
            state : Map(),
            input : Set(),
            nonce : 0,        // to be able to match requests and responses in the trace
            log : Set(),
            SIM_handles : Set()
        }        
    }

    action init : bool = all {
        g_clock::init(Set(),0,Set()),
        f_ac' = initial_f_ac
    }

    val Prop = true

}