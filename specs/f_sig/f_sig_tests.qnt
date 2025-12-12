module f_sig_tests{
    
    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"   
    import f_sig_types.* from "./f_sig_types"
    import f_sig.* from "./f_sig"
    import f_sig_env.* from "./f_sig_env" 


    run gen_cycle(cfg: MsgConfig) : bool = {
        (env_gen_reqs(cfg))
        .then(sig_step)
        .then(sig_gen_2_act) // forces simulator to act on the handle
        .then(sig_step)
        .then(env_consume_responses)        
    }    

    run sign_cycle(cfg: MsgConfig, msg: SIG_Document) : bool = {
        (env_sign_reqs_msg(cfg, msg))
        .then(sig_step)
        .then(sig_sign_2_act) // forces simulator to act on the handle
        .then(sig_step)
        .then(env_consume_responses)        
    }

    run verify_cycle(cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature) : bool = {
        (env_verify_reqs_args(cfg, vk, m, sig))
        .then(sig_step)
        .then(sig_verify_2_act) // forces simulator to act on the handle
        .then(sig_step)
        .then(env_consume_responses)        
    }
    
    run test1 = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then(gen_cycle(cfg)) // generate vk
        .then({ // sign 
            sign_cycle({...cfg, nonce: 2}, "Message 1")
        })
        .then({ // verify
            nondet gen_res = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            nondet sign_res = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
            verify_cycle({...cfg, nonce: 3}, gen_res.vk, "Message 1", sign_res.sig.unwrap())
        })
        .expect( // expect verification to be true
            nondet verify_res = f_sig_s.log.map(x => x.msg).sig_get_verify_response_messages().filter(x => x.cfg.nonce == 3).oneOf()
            verify_res.b == true
        )


        
    }
}