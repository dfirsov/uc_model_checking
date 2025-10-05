// -*- mode: Bluespec; -*-
module f_sig_ideal {

  import basicSpells.* from "../../spells/basicSpells"
  import choreo(processes = NODES) as choreo from "../../choreo"

  pure val FSig: Node = 1
  pure val ENV = 2
  pure val SIM = 3
  pure val NODES = Set(FSig,ENV,SIM)

  // - Disambiguate messages with salt(e.g., counter) to allow repeations (only when strictly nec.)
  // - Responsive/non-responsive simulator
  pure val honestParties = Set(1)
  pure val corruptedParties = Set(2)
  pure val allParties = honestParties.union(corruptedParties)
  pure val allVKs  = 1.to(5)
  pure val allSigs = 1.to(5)
  pure val allDocs = Set("doc1")


  var counter : int

  // Auxiliary types
  type Role = SigIF | SigSim | Env 

  type Party = int
  type PublicKey = int
  type Document = str
  type Signature = int

  type History = {
    msg_log: Set[Message],
  }  

  // Mandatory types for the Choreo
  type VerEntry = {
    vk  : PublicKey, 
    msg : Document, 
    sig : Signature,
    res : bool
  }

  type VKEntry = {
    party : Party,
    vk : PublicKey
  }

  type Node = int
  type Message =

    | GenRequest(Party) 
    | GenResponse(VKEntry)
    | SIMGen(Party)
    | SIMReleaseGen(VKEntry)

    | SignRequest({party:Party, msg : Document})
    | SignResponse({party:Party, ver_entry:VerEntry})
    | SIMSign({party:Party, msg:Document})  
    | SIMReleaseSign({party:Party,msg:Document,sig:Signature})
    | VerifyRequest({party:Party, vk:PublicKey, msg:Document, sig:Signature})
    | VerifyResponse({party:Party, ver_entry:VerEntry})
    | SIMVerify({party:Party, vk:PublicKey, msg:Document, sig:Signature})
    | SIMReleaseVerify({party:Party, ver_entry:VerEntry})


  type StateFields = {
    VK:   Set[VKEntry],
    Ver:  Set[VerEntry],
    role: Role,
  }

  type CustomEffects = ExcludeMsg((Node, Message)) | Log(Message)
  type Event = ()
  type Extensions = History

  /* Boilerplate */
  type LocalState = choreo::LocalState[Node, StateFields]
  type LocalContext = choreo::LocalContext[Node, StateFields, Message, Event, Extensions]
  type Transition = choreo::Transition[Node, StateFields, Message, Event, CustomEffects]
  type GlobalContext = choreo::GlobalContext[
    Node,
    StateFields,
    Message,
    Event,
    Extensions
  ]

  /* End of boilerplate */
        
  // --- Party transitions ----------------------------------------------

  pure def vk_generation_ideal1(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role != SigIF){
      Set()
    }else
      ctx.messages.filterMap(m => {
        match m {
          | GenRequest(party) => {
            if (ctx.state.VK.exists(x => x.party == party)){
              // finds the first one, also possible to traverse and generate multiple transitions
              val pk = ctx.state.VK.find(x => x.party == party).unwrap().vk

              Some({
                effects: Set( 
                    choreo::Send( ({ to: ENV, message: GenResponse({party: party, vk: pk}) })),
                    choreo::CustomEffect(ExcludeMsg((ctx.state.process_id, m)))
                            ),
                post_state: { ...ctx.state}
                   })
          // VK does not exist, send msg to SIM (no state to preserve)
          }else{
            Some({
              effects: Set( 
                choreo::Send(({ to: SIM, message: SIMGen(party)})),
                choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m) ) )),
              post_state: { ...ctx.state }
            })}}

      // receiving wrong messagess
      | _ => None
      }
    })
  } // messages := {GenRequest(1,counter), GenRequest(1, counter+1)} 


  pure def vk_generation_sim(ctx: LocalContext) : Set[Transition] = {  
    if (ctx.state.role == SigSim) { // only SIM can process this
      ctx.messages.map(m => {
        match m {
          | SIMGen(p) => {
            allVKs.map(vk =>
              Set({
                effects: Set( 
                    choreo::Send( ({ to: FSig, message: SIMReleaseGen({party: p, vk:vk}) })),
                    choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m)))
                  ),
                post_state: { ...ctx.state }
              })
            ).flatten()
          }
      | _ => Set()
      }
    }).flatten()
    }else{
      Set()
    }

  }

  pure def vk_generation_ideal2(ctx: LocalContext): Set[Transition] = {
    def filter_fun = (p) => 
      or {
        ctx.state.VK.exists(x => x.vk == p),
        ctx.state.Ver.exists(x => x.vk == p)
      }

    if(ctx.state.role != SigIF){
      Set()
    }else 
      ctx.messages.map(m => {
        match m {
          | SIMReleaseGen(p) => {
            {
              val party = p.party
              val vk = p.vk

              val san_vk = if(not (filter_fun(vk))){
                vk
              }else{
                allVKs.find(p => not(filter_fun(p))).unwrap()
              }          

          Set({
            effects: Set( 
              
                choreo::Send( ({ to: ENV, message: GenResponse({party:party, vk:san_vk}) } ) ),
                choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m) ) )
            ),
            post_state: { ...ctx.state, VK: ctx.state.VK.union(Set({party:party, vk:san_vk})),}
          })
          }
        }
        // TODO: report bug
      | _ => Set()
      }
      }).flatten()
    }


  pure def msg_sign_ideal1(ctx: LocalContext): Set[Transition] = {
    if(ctx.state.role != SigIF){
      Set()
    }else
      ctx.messages.map(m => {
        match m {
          | SignRequest(p) => {
            val party = p.party
            val msg = p.msg
            if(not (ctx.state.VK.exists(x => x.party == party))){
              Set()
            }else{
              Set({
                effects: Set( 
                    choreo::Send( ({ to: SIM, message: SIMSign({party:party, msg:msg})})),
                    choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m) ) ),
                    choreo::CustomEffect(Log( m ) ),
                ),
                post_state: { ...ctx.state }
              })
            }
          }
          | _ => Set()
        }
      }
      ).flatten()
  }

  pure def msg_sign_sim(ctx: LocalContext): Set[Transition] = {
    if(ctx.state.role != SigSim){
      Set()
    }else
      ctx.messages.map(m => {
        match m {
          | SIMSign(p) => {
            val party = p.party
            val msg = p.msg
            allSigs.map(sig => 
              Set({
                effects: Set( 
                    choreo::Send( ({ to: FSig, message: SIMReleaseSign({party:party, msg:msg, sig:sig})})),
                    choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m)))
                ),
                post_state: { ...ctx.state }
              })
            ).flatten()

          }
          | _ => Set()
        }
      }
      ).flatten()
  }

  pure def msg_sign_ideal2(ctx: LocalContext): Set[Transition] = {
    if(ctx.state.role != SigIF){
      Set()
    }else
      ctx.messages.map(m => {
        match m {
          | SIMReleaseSign(p) => {
            val party = p.party
            val msg = p.msg
            val sig = p.sig
            val vk = ctx.state.VK.find(x => x.party == party).unwrap().vk

            def filter_fun = (x) => 
              ctx.state.Ver.exists(y => y.vk == vk and
                                        y.msg == msg and
                                        y.sig == x)
            

            val san_sig = if (filter_fun(sig)){
              allSigs.find(x => not(filter_fun(x))).unwrap()
            }else{
              sig
            }
            Set({
              effects: Set( 
                  choreo::Send( ({ to: ENV, message: SignResponse({party:party, ver_entry:{vk: vk, msg: msg, sig: san_sig, res: true}}) } ) ),
                  choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m) ) )
              ),
              post_state: { ...ctx.state, Ver: ctx.state.Ver.union(Set({vk:vk, msg:msg, sig:san_sig, res:true})),}
            })
            }
          | _ => Set()
        }
      }
      ).flatten()
  }  


  pure def msg_verify_ideal1(ctx: LocalContext): Set[Transition] = {
    if(ctx.state.role != SigIF){
      Set()
    }else
      ctx.messages.map(m => {
        match m {
          | VerifyRequest(p) => {
            val party = p.party
            val vk = p.vk
            val msg = p.msg
            val sig = p.sig
            if(not (ctx.state.Ver.exists(x => x.vk == vk and x.msg == msg and x.sig == sig))){
              Set() // todo: send response
            }else{
              Set({
                effects: Set( 
                    choreo::Send( ({ to: SIM, message: SIMVerify({party:party, vk:vk, msg:msg, sig:sig})})),
                    choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m) ) )
                ),
                post_state: { ...ctx.state }
              })
            }
          }
          | _ => Set()
        }
      }
      ).flatten()
  }

  pure def msg_verify_sim(ctx: LocalContext): Set[Transition] = {
    if(ctx.state.role != SigSim){
      Set()
    }else
      ctx.messages.map(m => {
        match m {
          | SIMVerify(p) => {
            Set(true, false).map(res => 
              Set({
                effects: Set( 
                    choreo::Send( ({ to: FSig, message: SIMReleaseVerify({party:p.party, ver_entry:{vk:p.vk,msg:p.msg,sig:p.sig,res:res}})})),
                    choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m)))
                ),
                post_state: { ...ctx.state }
              })
            ).flatten()


          }
          | _ => Set()
        }
      }
      ).flatten()
  }

  pure def msg_verify_ideal2(ctx: LocalContext): Set[Transition] = {
    if(ctx.state.role != SigIF){
      Set()
    }else
      ctx.messages.map(m => {
        match m {
          | SIMReleaseVerify(q) => {
            val party = q.party
            val vk = q.ver_entry.vk
            val msg = q.ver_entry.msg
            val sig = q.ver_entry.sig
            val res = q.ver_entry.res

            val san_res = if(ctx.state.VK.exists(x => x.party.in(honestParties) and x.vk == vk)){
              false
            }else{
              res
            }

            Set({ 
              effects: Set( 
                  choreo::Send( ({ to: ENV, message: VerifyResponse({party:party, ver_entry:{vk:vk,msg:msg,sig:sig,res:san_res}} ) } ) ),
                  choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m) ) )
              ),
              post_state: { ...ctx.state, Ver: ctx.state.Ver.union(Set({vk:vk, msg:msg, sig:sig, res:san_res})),}
            })

          }
          | _ => Set()
        }
      }
      ).flatten()
  }


  // environment throws in verification requests
  pure def env_verify_msgs(ctx:LocalContext) : Set[Transition] = {
    if (ctx.state.role != Env ) {
      Set()
    }else{ // VerifyRequest((Party, PublicKey, Document, Signature))
      allParties.map(party => {
        allVKs.map(vk => {
          allDocs.map(msg => {
            allSigs.map(sig => {
            Set(
              { 
                effects: Set (
                  choreo::Send( ({ to: FSig, message: VerifyRequest({party:party,vk:vk,msg:msg,sig:sig} ) } ) )
                ), 
                post_state: { ...ctx.state }
              }
            )
            }).flatten()
          }).flatten()
        }).flatten()
      }).flatten()
    }
  }



  
  def apply_custom_effect(env: GlobalContext, effect: CustomEffects): GlobalContext = {
    match effect {
      | ExcludeMsg(p) => {... env, messages: env.messages.setBy(p._1, (old => old.exclude(Set(p._2)))) }
      | Log(msg) => {
          {...env, extensions: {...env.extensions, msg_log: env.extensions.msg_log.setAdd(msg)}}
        }
    }
  }

  pure def main_listener(ctx: LocalContext): Set[Transition] =
    Set(
      vk_generation_ideal1(ctx),
      vk_generation_ideal2(ctx),
      vk_generation_sim(ctx) 
    ).flatten()

  action gen_step1 = choreo::step(
    (ctx => vk_generation_ideal1(ctx)),
    apply_custom_effect
  )
  action gen_step2 = choreo::step(
    (ctx => vk_generation_ideal2(ctx)),
    apply_custom_effect
  )
  action gen_step3 = choreo::step(
    (ctx => vk_generation_sim(ctx)),
    apply_custom_effect
  )
  action sign_step1 = choreo::step(
    (ctx => msg_sign_ideal1(ctx)),
    apply_custom_effect
  )
  action sign_step2 = choreo::step(
    (ctx => msg_sign_ideal2(ctx)),
    apply_custom_effect
  )
  action sign_step3 = choreo::step(
    (ctx => msg_sign_sim(ctx)),
    apply_custom_effect
  )
  action verify_step1 = choreo::step(
    (ctx => msg_verify_ideal1(ctx)),
    apply_custom_effect
  )
  action verify_step2 = choreo::step(
    (ctx => msg_verify_ideal2(ctx)),
    apply_custom_effect
  )
  action verify_step3 = choreo::step(
    (ctx => msg_verify_sim(ctx)),
    apply_custom_effect
  )

  action env_step1 = choreo::step(
    (ctx => env_verify_msgs(ctx)),
    apply_custom_effect
  )
    

  action step = any {
    all { gen_step1, counter' = counter,},
    all { gen_step2, counter' = counter,},
    all { gen_step3, counter' = counter,},

    all { sign_step1, counter' = counter,},
    all { sign_step2, counter' = counter,},
    all { sign_step3, counter' = counter,},

    all { verify_step1, counter' = counter,},
    all { verify_step2, counter' = counter,},
    all { verify_step3, counter' = counter,},

    all { counter < 1,
          env_step1,
          counter' = counter + 1 }
  }

  pure def initialize(n: Node): LocalState = {
    {
      process_id: n,
      VK: Set(),
      Ver: Set(),
      role: if (n == FSig) SigIF else if (n == SIM) SigSim else Env
    }
  }

  pure val initial_history = {
    msg_log: Set(),
  }  

  action init = all {
    choreo::init({
      system: NODES.mapBy(n => initialize(n)),
      messages: NODES.mapBy(n => if (n == 1)  
         Set( GenRequest(1), 
              SignRequest({party:1, msg:"doc1"}),
          ) 
       else Set()),
    events: NODES.mapBy(n => Set()),
    extensions: initial_history
  })  ,
  counter' = 0

  }

  // --- Properties ----------------------------------------------------------- 
  // tlc: https://learntla.com/core/action-properties.html

  // this property is supposed to be violated to quickly check that system is "alive"
  val GenLiveness1 = choreo::s.system.get(FSig).VK.size() <= 2
  val SignLiveness1 = choreo::s.system.get(FSig).Ver.size() <= 1

  
  // Property: entries don't disappear from Ver for honest
  temporal VerOnlyGrows = always (
    // TODO: bug report (wrong compilation of "always" to both tlc and apalache)
    choreo::s.system.get(FSig).Ver.forall(x => {
      x.in(next(choreo::s.system.get(FSig).Ver))
    })
  )

  temporal VKOnlyGrows = always (
    // TODO: bug report (wrong compilation of "always" to both tlc and apalache)
    choreo::s.system.get(FSig).VK.forall(x => {
      x.in(next(choreo::s.system.get(FSig).VK))
    })
  )
  
  // Property: party has at most one verification key
  val OneVKperUser = allParties.forall(p => choreo::s.system.get(FSig).VK.filter(x => x.party == p).size() <= 1)

  // Property: Ver is consistent (FAILS, flip-flop is possible because of verify functionality)
  val VerConsistent = choreo::s.system.get(FSig).Ver.forall(entry => {
    val entry2 = {...entry, res : not(entry.res)}
     not (entry2.in(choreo::s.system.get(FSig).Ver))
  })

  // Property: verification keys are unique 
  val VKUnique = allVKs.forall(p => choreo::s.system.get(FSig).VK.filter(x => x.vk == p).size() <= 1)

  // Property(Fails): If Ver containts some vk entry  then VK table must  contain respective entry
  val VKVerConsistentDishonest = choreo::s.system.get(FSig).Ver.forall(
    entry => {
      choreo::s.system.get(FSig).VK.exists(p => p.vk == entry.vk)
    })


  // Propery: If Ver contains entry then environment received a respective response (true for honest, fails for dishonest)
  val VerifiesIfSignedHonest = choreo::s.system.get(FSig).Ver.forall(entry => {
      if(entry.res){
        val vkentry = choreo::s.system.get(FSig).VK.find((x) => x.vk == entry.vk)
        if(vkentry == None){
          true
        }else{
          val party = vkentry.unwrap().party
            
            // FAILS for corrupted partys 
            // choreo::s.messages.get(ENV).exists((m) => {
            //   m == SignResponse((party, entry))
            // }) 

          if(party.in(honestParties)){
            choreo::s.messages.get(ENV).exists((m) => {
              m == SignResponse({party:party, ver_entry: entry})
            }) 
          }else{
            true
          }
        }
      }else{
        true
      }
     
  })    

  // Property: if ENV received GenResponse then the key is in the set of verification keys
  val GenResponseValid = choreo::s.messages.get(ENV).forall(m => match m {
    | GenResponse(p) => choreo::s.system.get(FSig).VK.exists(x => x == p)
    | _ => true
  })  
  // Property: if SignResponse is received by ENV then Ver contains entry
  val SignResponseValid = choreo::s.messages.get(ENV).forall(m => match m {
    | SignResponse(p) => choreo::s.system.get(FSig).Ver.exists(x => x == p.ver_entry)
    | _ => true
  })
  // Property: if VerifyResponse is received then Ver contains the respective entry
  val VerifyResponseValid = choreo::s.messages.get(ENV).forall(m => match m {
    | VerifyResponse(p) => choreo::s.system.get(FSig).Ver.exists(x => x == p.ver_entry)
    | _ => true
  })



  // Property: Correctness -- Gen -> Sign -> Verify = Success (temporal)


  // Property: Correctness -- if all actions are weakly fair then GenResponse is eventually received
  temporal GenCorrectness = 
    gen_step1.weakFair(Set(choreo::s)).implies(
      gen_step2.weakFair(Set(choreo::s)).implies(
        gen_step3.weakFair(Set(choreo::s)).implies(
      choreo::s.messages.get(FSig).exists(m => match m {
        | GenRequest(p) => true
        | _ => false
      }
      ) implies (eventually (choreo::s.messages.get(ENV).exists(m => 
        match m {
          | GenResponse(p) => true
          | _ => false
        })))))
      
      )
}