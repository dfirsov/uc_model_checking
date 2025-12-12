module f_sig_env{


    import basicSpells.* from "../../spells/basicSpells"
    import f_sig_types.* from "./f_sig_types"
    import f_sig.* from "./f_sig"
    import common.* from "../common"    

   
   val Parties = Set(1,2,3)
   val Signers = Set(1,2,3)
   val Corrupted = Set(3)
   val sids = Set(1)

   val ALL_MSGs = Set(
       "Message 1",
       "Message 2",
       "Message 3",
       "Message 4",
       "Message 5",
   )

   val ALL_VKs = 1.to(10)
   val ALL_SIGs = 1.to(10)

    action step : bool = any {
        sig_step,  
        env_send_all_reqs,
        env_consume_responses,
    }

    pure def env_consume_response(sys: SIG_System) : Set[SIG_Transition] = {
         sys.input.sig_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                SIG_LogMessage(msg_log),
                SIG_UpdateInput(Set())
            )
         })
    }    

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(f_sig_s)
        
        all {
        // Non-deterministically choose one response message to consume
            response_msgs.size() > 0,
            nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
            f_sig_s' = sig_process_transitions(f_sig_s, transition)
        }
   }      

    action env_send_all_reqs : bool = all {
        nondet party = Parties.oneOf()
        nondet sid = sids.oneOf()
        val cfg = {party: party, sid: sid, nonce: f_sig_s.nonce}
        any {
            env_gen_reqs(cfg),
            env_sign_reqs(cfg),
            env_verify_reqs(cfg),
            env_sim_reqs,
        }
        
    }     

    action env_gen_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_sig_s.input.size() == 0,

        f_sig_s' = {...f_sig_s, 
            input: Set(GenRequest({cfg: cfg})),
            nonce: f_sig_s.nonce + 1

        }
    }    

    action env_sign_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_sig_s.input.size() == 0,

        nondet msg = ALL_MSGs.oneOf()

        f_sig_s' = {...f_sig_s, 
            input: Set(SignRequest({cfg: cfg, m: msg})),
            nonce: f_sig_s.nonce + 1
        }
    }

    action env_verify_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_sig_s.input.size() == 0,

        nondet msg = ALL_MSGs.oneOf()
        nondet sig = ALL_SIGs.oneOf()
        nondet vk  = ALL_VKs.oneOf()

        f_sig_s' = {...f_sig_s, 
            input: Set(VerifyRequest({cfg: cfg, vk: vk, m: msg, sig: sig}) ),
            nonce: f_sig_s.nonce + 1
        }
    }

    action env_sim_reqs: bool = all{
        f_sig_s.input.size() == 0,
        f_sig_s' = {...f_sig_s,
           input: Set(SIMControl),
        }
    }

    

    action init = all {
        sig_init(Parties, Corrupted, sids, Signers, ALL_VKs, ALL_SIGs),
    }    


}