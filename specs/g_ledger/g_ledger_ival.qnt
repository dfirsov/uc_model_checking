module g_ledger_ival {
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common" 
    import f_atms.* from "../f_atms/f_atms"
    import f_atms_types.* from "../f_atms/f_atms_types"    

    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"


    pure def led_eval_1(led: LED_System, atms: ATMS_System) : Set[(LED_Transition, ATMS_Transition)] = {
        led.input.led_get_eval_request_messages().map(p => {
            if(led_access_check(p.cfg, led.params)){
                if(p.log.listHas(p.tx)){
                    Set(({
                        post_state: led.state,
                        effects: Set(
                            LED_UpdateInput(Set(EvalResponse({cfg: p.cfg, log: p.log, tx: p.tx, b: false}))),
                            LED_LogMessage(EvalRequest(p))
                        )
                    }, atms.atms_id_trans())) 
                }else{
                    match p.tx {
                        | Normal(q) => {
                            Set(({
                                post_state: led.state,
                                effects: Set(
                                    LED_LogMessage(EvalRequest(p)),
                                    LED_UpdateInput(Set(EvalResponse({cfg: p.cfg, log: p.log, tx: p.tx, b: q.b}))),
                                )
                            }, atms_id_trans(atms)))
                        }

                        | Install(q) => {
                            val log_check = p.log.indices().map(i => p.log[i].led_get_install_tx()).flatten().filter(x => x.code == q.code).size() == 0
                            if(log_check){
                                Set(({
                                    post_state: {...led.state,
                                        mem: led.state.mem.put(
                                            q.cID,
                                            {
                                                hs: List(),
                                                avk: None
                                            }
                                        )
                                    },
                                    effects: Set(
                                        LED_LogMessage(EvalRequest(p)),                                        
                                        LED_UpdateInput(Set(EvalResponse({cfg: p.cfg, log: p.log, tx: p.tx, b: true}))),
                                    )
                                }, atms.atms_id_trans()))                                
                            }else{
                                Set(({
                                    post_state: led.state,
                                    effects: Set(
                                        LED_LogMessage(EvalRequest(p)), 
                                        LED_UpdateInput(Set(EvalResponse({cfg: p.cfg, log: p.log, tx: p.tx, b: false}))),
                                    )
                                }, atms.atms_id_trans()))                                
                            }
                        }

                        | Update(q) => {
                            val install_txs = p.log.indices().map(i => p.log[i].led_get_install_tx()).flatten()
                            if(install_txs.size() > 0){
                                val tx_data = install_txs.find(x => true).unwrap()
                                val mem = led.state.mem.get(tx_data.cID)
                                val maybe_avk = mem.avk
                                
                                val avk = if (maybe_avk != None) {
                                    maybe_avk.unwrap()
                                } else {
                                    tx_data.code  
                                }

                                Set(({
                                    post_state: led.state,
                                    effects: Set(
                                        LED_LogMessage(EvalRequest(p)),
                                        LED_UpdateInput(Set()),
                                        LED_AppendHandle(EvalAverWait({cfg: p.cfg, log: p.log, tx: p.tx, cID: tx_data.cID, avk: avk, msg: q.msg})),
                                    )
                                }, {
                                    post_state: atms.state,
                                    effects: Set(
                                        ATMS_UpdateInput(Set(AVerifyRequest({cfg: p.cfg, avk: avk, m: q.msg, sig: q.sigma})))
                                    ),
                                }))
                            }else{
                                Set()
                            }
                        }
                    }
                }
            }else{
                Set(({
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set()),
                    )
                }, atms.atms_id_trans()))             
            }
        }).flatten()
    }  

    pure def led_eval_2(led: LED_System, atms: ATMS_System) : Set[(LED_Transition, ATMS_Transition)] = {    
        atms.input.atms_get_averify_response_messages().map(p => {
            led.handles.led_get_eval_aver_wait_messages().filter(h => p.cfg == h.cfg).map(q => {
                if(p.b){
                    Set(({
                        post_state: {...led.state,
                            mem: led.state.mem.put(
                                q.cID,
                                {
                                    hs: led.state.mem.get(q.cID).hs.append(q.msg.h),
                                    avk: Some(q.msg.avk)
                                }
                            )
                        },
                        effects: Set(
                            LED_UpdateInput(Set(EvalResponse({cfg: q.cfg, log: q.log, tx: q.tx, b: true}))),
                            LED_RemoveHandle(EvalAverWait(q)),
                        )
                    }, {
                        post_state: atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set())
                        ),
                    })) 
                }else{
                    Set(({
                        post_state: led.state,
                        effects: Set(
                            LED_UpdateInput(Set(EvalResponse({cfg: q.cfg, log: q.log, tx: q.tx, b: false}))),
                            LED_RemoveHandle(EvalAverWait(q)),
                        )
                    }, {
                        post_state: atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set())
                        ),
                    })) 
                }
            }).flatten()
        }).flatten()
    }  




    pure def led_iread_1(led : LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_read_request_messages().filter(p => led_access_check(p.cfg, led.params)).map(p => {
            {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set(LED_SIMControl)),
                    LED_AppendHandle(ReadSIMHold(p)),
                    LED_LogMessage(ReadRequest(p))
                )
            }
        }).map(t => (t, clock.gc_id_trans()))
    }
 
    pure def led_iread_2(led : LED_System, clock: GC_System, simI : List[Pointer], simPtr_prim:Pointer) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_sim_control_messages().map(_ => {  
            led.handles.led_get_read_sim_hold_messages().map(p => {
                {
                    post_state: led.state,
                    effects: Set(
                        LED_RemoveHandle(ReadSIMHold(p)),
                        LED_UpdateInput(Set(ReadSIMRelease({cfg:p.cfg, I: simI, ptr_prim: simPtr_prim}))) 
                    )
                }
            })
        }).flatten().map(t => (t, gc_id_trans(clock)))        
    }

    action led_iread_2_act = {
        all {
            nondet simI_set = 1.to(10).powerset().oneOf() // TODO: determine appropriate size
            val simI = simI_set.toList((a,b) => EQ)
            nondet simPtr_prim = 1.to(10).oneOf() // TODO: what is the right range here?

            val ts = led_iread_2(g_ledger_s, g_clock_s, simI, simPtr_prim)
            all {
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s' = led_process_transitions(g_ledger_s, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }

    // maybe refactor that for the direct response
    pure def led_iread_3(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_read_sim_release_messages().map(p => {

            val led_transition = {
                post_state: led.state,
                effects: Set(
                    LED_UpdateInput(Set()),
                    LED_AppendHandle(ReadTimeRequestHold({cfg: p.cfg, I: p.I, ptr_prim: p.ptr_prim}))
                )
            }

            val gc_transition = {
                post_state: clock.state,
                effects: Set(
                    GC_UpdateInput(Set(TimeRequest({cfg: p.cfg})))
                )
            }

            (led_transition, gc_transition)
        })
    }

    pure def led_iread_4(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        clock.input.get_time_response_messages().map(q => {
            led.handles.led_get_read_time_request_hold_messages().filter(p => p.cfg.nonce == q.cfg.nonce).map(p => {

                val t = q.time
                val P = p.cfg.party
                val I = p.I    // simulators chosen I
                
                val X = led_san_I(led, I, t)
 
                val led_transition = {
                    post_state: led.state, 
                    effects: Set(
                        LED_RemoveHandle(ReadTimeRequestHold(p)), 
                        LED_UpdateInput(Set(ReadLoopComputation({cfg: p.cfg, ptr_prim: p.ptr_prim, t: t, I: X, buffer: led.state.buffer, log: led.state.log})))
                    )
                }

                val gc_transition = {
                    post_state: clock.state,
                    effects: Set(
                        GC_UpdateInput(Set())
                    )
                }     

                (led_transition, gc_transition)

          })
        }).flatten()
    }

    pure def led_iread_5(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_read_loop_computation_messages().map(p => {

            if(p.I == List()){
                {
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set(ReadLoopComputationFinish({cfg: p.cfg, ptr_prim: p.ptr_prim, buffer: p.buffer, log: p.log, t: p.t}))),
                    )
                }
            }else{
                val i = p.I.head()
                if(p.buffer.has(i)){
                    val tx = p.buffer.get(i).tx
                    {
                        post_state: led.state,
                        effects: Set(
                            LED_UpdateInput(Set(EvalRequest({cfg: p.cfg, tx: tx, log: p.log.listMap(b => b.tx)}))),
                            LED_AppendHandle(ReadLoopComputation(p))
                        )
                    }
                }else{
                    {
                        post_state: led.state,
                        effects: Set(
                            LED_UpdateInput(Set(ReadLoopComputation({...p, I: p.I.tail()}))),
                        )
                    }
                }
            }
        }).map(t => (t, gc_id_trans(clock)))
    }

    pure def led_iread_6(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_eval_response_messages().map(p => {
            led.handles.led_get_read_loop_computation_messages().filter(h => p.cfg == h.cfg).map(q => {

                val tx_t : {tx:LED_Tx, time: Time} = q.buffer.get(q.I.head())
                val new_log = if(p.b) q.log.append(tx_t) else q.log
                val new_buffer =  q.buffer.mapRemove(q.I.head())
                val new_I = q.I.tail()

                {
                    post_state: {...led.state,
                        log: new_log,
                        buffer: new_buffer,
                    },
                    effects: Set(
                        LED_UpdateInput(Set(ReadLoopComputation({cfg: p.cfg, ptr_prim: q.ptr_prim, t: q.t, I: new_I, buffer: new_buffer, log: new_log}))),
                        LED_RemoveHandle(ReadLoopComputation(q))
                    )
                }
            })
        }).flatten().map(t => (t, gc_id_trans(clock)))
    }

    pure def led_iread_7(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.led_get_read_loop_computation_finish_messages().map(p => {

                val log = p.log
                val buffer = p.buffer
                val P = p.cfg.party
                val t = p.t
                val ptr_prim = p.ptr_prim
 
                val ptrP = led_san_ptr(ptr_prim, P, led.params.delta_slack, log, led.state.ptr, t)
 
                val led_transition = {
                    if(log == List() or ptrP == None){
                        {
                            post_state: {...led.state, // ptr unchanged
                                buffer: buffer,
                                log: log,
                            },
                            effects: Set(
                                LED_UpdateInput(Set(ReadResponse({cfg: p.cfg, result: List()})))
                            )
                        }
                    }else{
                        val ptr = led.state.ptr.put(P, ptrP.unwrap())
                        val result = log.mySlice(0,ptr.get(P)+1).listMap(b => b.tx) // TODO: check off-by-one

                        {
                            post_state: {...led.state, 
                                buffer: buffer,
                                log: log,
                                ptr: ptr
                            },
                            effects: Set(
                                LED_UpdateInput(Set(ReadResponse({cfg: p.cfg, result: result})))
                            )
                        }
                    }
                }

                led_transition

        }).map(t => (t, gc_id_trans(clock)))
    }

   def led_main_listener_ival(led: LED_System, clock: GC_System, atms: ATMS_System) : (Set[(LED_Transition, GC_Transition, ATMS_Transition)]) = {
        Set(
            led_eval_1(led, atms),
            led_eval_2(led, atms),
        ).flatten().map(t => (t._1, gc_id_trans(clock), t._2))
        .union(
            Set(led_iread_7(led, clock),
                led_iread_6(led, clock),
                led_iread_5(led, clock),
                led_iread_4(led, clock),
                led_iread_3(led, clock),
            // led_iread_2 is modelled as an action
                led_iread_1(led, clock),
            ).flatten().map(t => (t._1, t._2, atms.atms_id_trans()))
        )
    }

    action led_gc_atms_step_with(q: (LED_System, GC_System, ATMS_System) => Set[(LED_Transition, GC_Transition, ATMS_Transition)]): bool = {
        all{
            val ts = q(g_ledger_s, g_clock_s, f_atms_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s' = led_process_transitions(g_ledger_s, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2),
                    f_atms_s' = atms_process_transitions(f_atms_s, transition._3)
                }
            }
        }
    }     

    action led_step_ival : bool =  any {
        led_gc_atms_step_with(led_main_listener_ival),
        all{
            led_iread_2_act,
            f_atms_s' = f_atms_s,
        }
    }            

}