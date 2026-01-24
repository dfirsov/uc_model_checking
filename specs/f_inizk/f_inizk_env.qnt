module f_inizk_env{


    import basicSpells.* from "../../spells/basicSpells"
    import f_inizk_types.* from "./f_inizk_types"
    import f_inizk.* from "./f_inizk"
    import f_hash_types.* from "../f_hash/f_hash_types"
    import f_hash.* from "../f_hash/f_hash"
    import common.* from "../common"    

   
   val Parties = Set(1,2,3)
   val Corrupted = Set(3)
   val sids = Set(1)
   val Rel = Set(
       ("Statement 1", 1),
       ("Statement 2", 2),
       ("Statement 3", 3),
       ("Statement 3", 7),
       ("Statement 4", 4),
       ("Statement 4", 3),
       ("Statement 5", 5),
   )

   val ALL_PROOFS = 1.to(10)
   val ALL_STATEMENTS = Set(
       "Statement 1",
       "Statement 2",
       "Statement 3",
       "Statement 4",
       "Statement 5",
    )
    val ALL_WITNESSES = 1.to(10)


    action step : bool = any {
        nizk_step,  
        all{
            hash_step,
            f_nizk_s' = f_nizk_s
        },
        env_send_all_reqs,
        env_consume_responses,
        //env_give_control_to_simulator,
    }

    pure def env_consume_response(sys: NIZK_System) : Set[NIZK_Transition] = {
         sys.input.nizk_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                NIZK_LogMessage(msg_log),
                NIZK_UpdateInput(Set())
            )
         })
    }    

    action env_give_control_to_simulator : bool = all {
        // Precondition: input must be empty
        f_nizk_s.input.size() == 0,

        f_nizk_s' = {...f_nizk_s, 
            input: Set(NIZK_SIMControl),
        }
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(f_nizk_s)
        
        all {
        // Non-deterministically choose one response message to consume
            nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
            f_nizk_s' = nizk_process_transitions(f_nizk_s, transition),
            f_hash_s' = f_hash_s
        }
   }      

    action env_send_all_reqs : bool = all {
        f_nizk_s.input.size() == 0 and f_hash_s.input.size() == 0,

        nondet party = Parties.oneOf()
        nondet sid = sids.oneOf()
        val cfg = {party: party, sid: sid, nonce: f_nizk_s.nonce}
        any {
            env_prove_reqs(cfg),
            env_verify_reqs(cfg),
            //env_sim_reqs,
        },
        f_hash_s' = f_hash_s
        
    }     

    action env_prove_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_nizk_s.input.size() == 0,

        nondet x = ALL_STATEMENTS.oneOf()
        nondet w = ALL_WITNESSES.oneOf()
        env_prove_reqs_args(cfg, x, w)
    }    

    action env_prove_reqs_args(cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness) : bool = all {
        f_nizk_s' = {...f_nizk_s, 
            input: Set(NIZK_ProveRequest({cfg: cfg, x: x, w: w}) ),
            nonce: f_nizk_s.nonce + 1
        }
    }

    action env_verify_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_nizk_s.input.size() == 0,

        nondet x = ALL_STATEMENTS.oneOf()
        nondet pi = ALL_PROOFS.oneOf()
        env_verify_reqs_args(cfg, x, pi)
    }

    action env_verify_reqs_args(cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof) : bool = all {
        f_nizk_s' = {...f_nizk_s, 
            input: Set(NIZK_VerifyRequest({cfg: cfg, x: x, pi: pi}) ),
            nonce: f_nizk_s.nonce + 1
        }
    }

    action env_sim_reqs: bool = all{
        f_nizk_s.input.size() == 0,
        f_nizk_s' = {...f_nizk_s,
           input: Set(NIZK_SIMControl),
        }
    }

    
    action init = all {
        nizk_init(Parties, Corrupted, sids, ALL_PROOFS, ALL_WITNESSES, Rel),
        hash_init(Parties, sids, 1.to(100)),
    }    

}