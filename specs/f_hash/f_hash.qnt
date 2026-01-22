module f_hash {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    
    import common.* from "../common"
    
    import f_hash_types.* from "./f_hash_types"

    var f_hash_s : HASH_System

    val HASH_ALL_HASHES = 50.to(150)


    pure def hash_hash_1(f_hash: HASH_System) : Set[HASH_Transition] = {
        f_hash.input.hash_get_hash_request_messages().map(p => {
                {
                post_state: f_hash.state,
                effects: Set(
                    HASH_LogMessage(HashRequest(p)),
                    HASH_AppendHandle(SIMHashRequestHold(p)),
                    HASH_UpdateInput(Set(HASH_SIMControl))
                    )
                }
        })
    }
    
    action hash_hash_2_act : bool = {
        all{
            nondet sim_h = HASH_ALL_HASHES.oneOf()
            val ts = hash_hash_2(f_hash_s, sim_h)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_hash_s' = hash_process_transitions(f_hash_s, transition)
                }
            }
        }
    }

    pure def hash_hash_2(f_hash: HASH_System, sim_h : Hash_Output) : Set[HASH_Transition] = {
        f_hash.input.hash_get_sim_control_messages().map(_ => {
            f_hash.handles.hash_get_sim_request_hold_messages().map(p => {
                Set({
                    post_state: f_hash.state,
                    effects: Set(
                        HASH_RemoveHandle(SIMHashRequestHold(p)),
                        HASH_UpdateInput(Set(SIMHashRequestRelease({cfg: p.cfg, x: p.x, sim_h: sim_h})))
                    )
                })
            }).flatten()
        }).flatten()
    }

    pure def san_hash(h: Hash_Output, f_hash_s: HASH_System, sid: SID) : Option[Hash_Output] = {
        if(clean_hash(h, f_hash_s.state.get(sid).hash_map)){
            Some(h)
        } else {
            HASH_ALL_HASHES.find(hh => clean_hash(hh, f_hash_s.state.get(sid).hash_map))
        }
    }

    pure def clean_hash(h: Hash_Output, hash_map : Hash_Input -> Hash_Output) : bool = {
        hash_map.values().forall(existing_h => existing_h != h)
    }

    pure def hash_hash_3(f_hash: HASH_System) : Set[HASH_Transition] = {
        f_hash.input.hash_get_sim_request_release_messages().map(p => {

            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = f_hash.state.get(sid)

            if(sid_state.hash_map.has(p.x)){
                {
                    post_state: f_hash.state,
                    effects: Set(
                        HASH_UpdateInput(Set(HashResponse({cfg: p.cfg, x: p.x, h: sid_state.hash_map.get(p.x)})))
                    )
                }
            }else{
                val maybe_hash = san_hash(p.sim_h, f_hash, sid)
                if(maybe_hash == None){
                    {
                        post_state: f_hash.state,
                        effects: Set(
                            HASH_UpdateInput(Set()) // 
                        )
                    }
                }else{
                    val hash = maybe_hash.unwrap()
                    {
                        post_state: f_hash.state.put(sid, {hash_map: sid_state.hash_map.put(p.x, hash)}),
                        effects: Set(
                            HASH_UpdateInput(Set(HashResponse({cfg: p.cfg, x: p.x, h: hash}))),
                        )
                    }
                }
            }   

        })
    }   

    pure def hash_sim_loses_ctrl(hash: HASH_System) : Set[HASH_Transition] = {
        hash.input.hash_get_sim_control_messages().map(c =>  
            {
                post_state : hash.state,
                effects : Set(
                    HASH_UpdateInput(Set()),
                    )
            })
    }    

    def hash_main_listener(crs: HASH_System) : Set[(HASH_Transition)] = {
        Set(
            hash_hash_1(crs),
            hash_hash_3(crs),
            //hash_sim_loses_ctrl(crs)
        ).flatten()
    }     

    def hash_process_transitions(sys: HASH_System, transition: HASH_Transition) : HASH_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => hash_apply_effect(a1,a2)))
    }     

    action hash_step_with(q: (HASH_System) => Set[(HASH_Transition)]): bool = {
        all{
            val ts = q(f_hash_s)
            all {
                ts.size() > 0,
                nondet transition = ts.oneOf()
                all {
                    f_hash_s' = hash_process_transitions(f_hash_s, transition)
                }
            }
        }
    } 


    action hash_step : bool =  any {
        hash_hash_2_act,
        hash_step_with(hash_main_listener)
    }        


    pure def hash_initial(P: Set[Party], SIDs: Set[SID]) : HASH_System = {
        {
            params : {
                P: P,
                SIDs: SIDs,
            },
            state: SIDs.mapBy(sid => {
                hash_map: Map(),
            }),
            input: Set(),
            nonce: 0, 
            log: Set(),
            handles: Set()
        }
    }

    action hash_init(P: Set[Party], SIDs: Set[SID]) : bool = all {
        f_hash_s' = hash_initial(P, SIDs)
    }   

}