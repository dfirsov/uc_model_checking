module f_nizk_env{


    import basicSpells.* from "../../spells/basicSpells"
    import f_nizk_types.* from "./f_nizk_types"
    import f_nizk.* from "./f_nizk"
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
        env_send_all_reqs,
        env_consume_responses,
        env_give_control_to_simulator,
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
            input: Set(SIMControl),
        }
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(f_nizk_s)
        
        all {
        // Non-deterministically choose one response message to consume
            response_msgs.size() > 0,
            nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
            f_nizk_s' = nizk_process_transitions(f_nizk_s, transition)
        }
   }      

    action env_send_all_reqs : bool = all {
        nondet party = Parties.oneOf()
        nondet sid = sids.oneOf()
        val cfg = {party: party, sid: sid, nonce: f_nizk_s.nonce}
        any {
            env_prove_reqs(cfg),
            env_verify_reqs(cfg),
            env_sim_reqs,
        }
        
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
            input: Set(ProveRequest({cfg: cfg, x: x, w: w}) ),
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
            input: Set(VerifyRequest({cfg: cfg, x: x, pi: pi}) ),
            nonce: f_nizk_s.nonce + 1
        }
    }

    action env_sim_reqs: bool = all{
        f_nizk_s.input.size() == 0,
        f_nizk_s' = {...f_nizk_s,
           input: Set(SIMControl),
        }
    }

    
    action init = all {
        nizk_init(Parties, Corrupted, sids, ALL_PROOFS, ALL_WITNESSES, Rel),
    }    

}