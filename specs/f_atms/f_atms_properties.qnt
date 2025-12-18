module f_atms_properties{
    
    import basicSpells.* from "../../spells/basicSpells"
    import f_atms_types.* from "./f_atms_types"
    import f_atms.* from "./f_atms"
    import f_atms_env.* from "./f_atms_env"
    import common.* from "../common"    
   

    val P1 = f_atms_s.log.size() <= 10
    val P2 = f_atms_s.log.map(x => x.msg).atms_get_gen_response_messages().size() <= 2
    val P3 = f_atms_s.log.map(x => x.msg).atms_get_akey_response_messages().size() <= 2
    val P4 = f_atms_s.log.map(x => x.msg).atms_get_acheck_response_messages().size() <= 2
    val P5 = f_atms_s.log.map(x => x.msg).atms_get_sign_response_messages().size() <= 2
    val P6 = f_atms_s.log.map(x => x.msg).atms_get_verify_response_messages().size() <= 2
    val P7 = f_atms_s.log.map(x => x.msg).atms_get_asign_response_messages().size() <= 2
    val P8 = f_atms_s.log.map(x => x.msg).atms_get_averify_response_messages().size() <= 2

    
}