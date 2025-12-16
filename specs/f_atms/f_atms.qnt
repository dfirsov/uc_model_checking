module f_atms{


    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"   
    import f_atms_types.* from "./f_atms_types"


    var f_atms_s : ATMS_System

    pure def atms_gen_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.atms_get_gen_request_messages().map(p =>{

            if(atms_access_check(p.cfg, atms.params)) {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMGenHold(p))
                    )
                })                
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    }    

    pure def atms_gen_2(atms: ATMS_System, sim_vk: Option[ATMS_VK]): Set[ATMS_Transition] = {
        atms.input.atms_get_sim_control_messages().map(_ => {
            atms.handles.atms_get_sim_gen_hold_messages().map(p => {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMGenRelease({cfg: p.cfg, sim_vk: sim_vk}))),
                        ATMS_RemoveHandle(SIMGenHold(p))
                    )
                }
            })
        }).flatten()
    }

    pure def atms_gen_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.atms_get_sim_gen_release_messages().map(p => {
            val P = p.cfg.party
            val sid = p.cfg.sid
            val sid_state = atms.state.get(sid)
            val maybe_vk = sid_state.VK.find(x => x.party == P)
            if(maybe_vk != None){
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(GenResponse({cfg: p.cfg, vk: maybe_vk.unwrap().vk}))),
                    )
                })
            }else{
                def san_predicate = (vk) => { 
                    and {
                        not (sid_state.VK.exists(x => x.party != P)),
                        or {
                            not (sid_state.VER.exists(x => x.vk != vk)),
                            sid_state.VER.exists(x => x.vk == vk and x.b == false)
                        }
                    }
                }

                val maybe_new_vk = if(p.sim_vk != None and san_predicate(p.sim_vk.unwrap())){
                    p.sim_vk
                }else{ 
                    atms.params.ALL_VKS.find(vk => san_predicate(vk))
                }

                if(maybe_new_vk != None){
                    Set({
                        post_state: atms.state.set(sid, {...sid_state,
                            VK: sid_state.VK.setAdd({party: P, vk: maybe_new_vk.unwrap()})
                        }),
                        effects: Set(
                            ATMS_UpdateInput(Set(GenResponse({cfg: p.cfg, vk: maybe_new_vk.unwrap()}))),
                        )
                    })
                }else{
                    Set() // run out of VKS
                }
            }
        }).flatten()
    }


    pure def atms_akey_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.atms_get_akey_request_messages().map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMAKeyHold(p))
                    )
                })                
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    }    

    pure def atms_akey_2(atms: ATMS_System, sim_avk: Option[ATMS_AVK]): Set[ATMS_Transition] = {
        atms.input.atms_get_sim_control_messages().map(_ => {
            atms.handles.atms_get_sim_akey_hold_messages().map(p => {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMAKeyRelease({cfg: p.cfg, vks: p.vks, sim_avk: sim_avk}))),
                        ATMS_RemoveHandle(SIMAKeyHold(p))
                    )
                }
            })
        }).flatten()
    }

    pure def san_avk(cfg: MsgConfig, atms: ATMS_System, avk: Option[ATMS_AVK]) : Option[ATMS_AVK] = {
        val sid_state = atms.state.get(cfg.sid)
        def san_predicate = (avk) => {
            and {
                avk != None,
                not (sid_state.AVK.exists(x => x.avk != avk.unwrap())),
                or {
                    not (sid_state.AVER.exists(x => x.avk != avk.unwrap())),
                    sid_state.AVER.exists(x => x.avk == avk.unwrap() and x.b == false)
                }
            }
        }

        if(san_predicate(avk)){
            avk
        }else{ 
            atms.params.ALL_AVKS.find(avk => san_predicate(Some(avk)))
        }
    }

    pure def atms_akey_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.atms_get_sim_akey_release_messages().map(p => {
            val P = p.cfg.party
            val sid = p.cfg.sid
            val sid_state = atms.state.get(sid)
            val maybe_avk = sid_state.AVK.find(x => x.vks == p.vks)
            if(maybe_avk != None){
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(AKeyResponse({cfg: p.cfg, avk: maybe_avk.unwrap().avk}))),
                    )
                })
            }else{
                val maybe_new_avk = san_avk(p.cfg, atms, p.sim_avk)
                if (maybe_new_avk != None){
                    val new_avk = maybe_new_avk.unwrap()
                    Set({
                        post_state: atms.state.set(sid, {...sid_state,
                            AVK: sid_state.AVK.setAdd({vks: p.vks, avk: new_avk}),
                            VKs: sid_state.VKs.setAdd({avk: new_avk, vks: p.vks})
                        }),
                        effects: Set(
                            ATMS_UpdateInput(Set(AKeyResponse({cfg: p.cfg, avk: new_avk}))),
                        )
                    })
                }else{
                    Set() // run out of AVKS
                }
            }
        }).flatten()
    }

    pure def atms_acheck_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.atms_get_acheck_request_messages().map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMACheckHold(p))
                    )
                })             
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    }
 
    pure def atms_acheck_2(atms: ATMS_System, sim_avk: Option[ATMS_AVK]): Set[ATMS_Transition] = {
        atms.input.atms_get_sim_control_messages().map(_ => {
            atms.handles.atms_get_sim_acheck_hold_messages().map(p => {
                val P = p.cfg.party
                val sid_state = atms.state.get(p.cfg.sid)
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMACheckRelease({cfg: p.cfg, vks: p.vks, avk:p.avk, sim_avk: sim_avk}))),
                        ATMS_RemoveHandle(SIMACheckHold(p))
                    )
                })
            }).flatten()
        }).flatten()
    }

    pure def atms_acheck_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.atms_get_sim_acheck_release_messages().map(p => {
            val sid_state = atms.state.get(p.cfg.sid)
            val maybe_avk_record = sid_state.AVK.find(x => x.vks == p.vks)

            if(maybe_avk_record != None){
                val avk = maybe_avk_record.unwrap().avk
                val b = p.avk == avk
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(ACheckResponse({cfg: p.cfg,  b: b}))),
                    )
                })
            }else{

                val san_sim_avk_maybe = san_avk(p.cfg, atms, p.sim_avk)

                val new_AVK = if(san_sim_avk_maybe != None){
                    sid_state.AVK.setAdd({vks: p.vks, avk: san_sim_avk_maybe.unwrap()})
                }else{
                    sid_state.AVK
                }
                val new_VKs = if(san_sim_avk_maybe != None){
                    sid_state.VKs.setAdd({avk: san_sim_avk_maybe.unwrap(), vks: p.vks})
                }else{
                    sid_state.VKs
                }

                val b = if(san_sim_avk_maybe != None){
                    p.avk == san_sim_avk_maybe.unwrap()
                }else{
                    false
                }


                Set({
                    post_state: atms.state.set(p.cfg.sid, {...sid_state,
                        AVK: new_AVK,
                        VKs: new_VKs
                    }),
                    effects: Set(
                        ATMS_UpdateInput(Set(ACheckResponse({cfg: p.cfg,  b: b}))),
                    )
                })
            }
        }).flatten()
    }

    pure def atms_sign_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.atms_get_sign_request_messages().map(p =>{
            if(atms_access_check(p.cfg, atms.params) and atms.state.get(p.cfg.sid).VK.exists(x => x.party == p.cfg.party)) {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMSignHold(p))
                    )
                })
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    }

    pure def atms_sign_2(atms: ATMS_System, sim_sig: Option[ATMS_Signature]): Set[ATMS_Transition] = {
        atms.input.atms_get_sim_control_messages().map(_ => {
            atms.handles.atms_get_sim_sign_hold_messages().map(p => {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMSignRelease({cfg: p.cfg, m: p.m, sim_sig: sim_sig}))),
                        ATMS_RemoveHandle(SIMSignHold(p))
                    )
                }
            })
        }).flatten()
    }

    pure def atms_sign_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.atms_get_sim_sign_release_messages().map(p => {
            val sid_state = atms.state.get(p.cfg.sid)
            // runtime error if not true
            val vk_record = sid_state.VK.find(x => x.party == p.cfg.party).unwrap()

            def san_predicate = (sig) => {
                and{
                    sig != None,
                    or{
                        not (sid_state.VER.exists(x => and {
                            x.m == p.m,
                            x.sig == sig.unwrap(),
                            x.vk == vk_record.vk
                        })),
                        sid_state.VER.exists(x => and {
                            x.m == p.m,
                            x.sig == sig.unwrap(),
                            x.vk == vk_record.vk,
                            x.b == true
                        })
                    }
                }
            }

            val maybe_new_sig = if(san_predicate(p.sim_sig)){
                p.sim_sig
            }else{  
                atms.params.ALL_SIGS.find(sig => san_predicate(Some(sig)))
            }

            if(maybe_new_sig != None){
                val new_sig = maybe_new_sig.unwrap()
                Set({
                    post_state: atms.state.set(p.cfg.sid, {...sid_state, 
                        Signers: sid_state.Signers.setAdd({m: p.m, signer: vk_record.vk}),
                        VER: sid_state.VER.setAdd({vk: vk_record.vk, m: p.m, sig: new_sig, b: true})
                    }),
                    effects: Set(
                        ATMS_UpdateInput(Set(SignResponse({cfg: p.cfg, m: p.m, sig: new_sig}))),
                    )
                })
            }else{
                Set()
            }
            
        }).flatten()

    }

    pure def atms_ver_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.atms_get_verify_request_messages().map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                val sid_state = atms.state.get(p.cfg.sid)
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMVerifyHold(p))
                    )
                })
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    }

    pure def atms_ver_2(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.atms_get_sim_control_messages().map(_ => {
            atms.handles.atms_get_sim_verify_hold_messages().map(p => {
                Set(true,false).map(b => {
                        post_state: atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set(SIMVerifyRelease({cfg: p.cfg, vk: p.vk, m: p.m, sig: p.sig, b: b}))),
                            ATMS_RemoveHandle(SIMVerifyHold(p))
                        )
                })
            }).flatten()
        }).flatten()
    }

    pure def atms_ver_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.atms_get_sim_verify_release_messages().map(p => {
            val sid_state = atms.state.get(p.cfg.sid)

            val ver_record = sid_state.VER.find(x => and {
                x.vk == p.vk,
                x.m == p.m,
                x.sig == p.sig,
            })

            if(ver_record != None){
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(VerifyResponse({cfg: p.cfg, vk: p.vk, m: p.m, sig: p.sig,  b: ver_record.unwrap().b}))),
                    )
                })
            }else{
                val new_b = if (atms.params.P.exists(P => {
                    and {
                        not (P.in(atms.params.C)),
                        sid_state.VK.exists(x => x.party == P)
                    }
                })){
                    false
                }else{
                    p.b
                }

                Set({
                    post_state: atms.state.set(p.cfg.sid, {...sid_state,
                        VER: sid_state.VER.setAdd({vk: p.vk, m: p.m, sig: p.sig, b: new_b})
                    }),
                    effects: Set(
                        ATMS_UpdateInput(Set(VerifyResponse({cfg: p.cfg, vk: p.vk, m: p.m, sig: p.sig, b: new_b}))),
                    )
                })
            }
        }).flatten()
    }

    pure def atms_asign_1(atms : ATMS_System) : Set[ATMS_Transition] = {
        Set() // To be implemented
    }



    
}