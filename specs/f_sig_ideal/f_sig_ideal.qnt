// -*- mode: Bluespec; -*-
module f_sig_ideal {
  import basicSpells.* from "./spells/basicSpells"
  import choreo(processes = NODES) as choreo from "./choreo"

  pure val FSig: Node = 1
  pure val ENV = 2
  pure val SIM = 3
  pure val NODES =Set(FSig,ENV,SIM)

  // - Disambiguate messages with counter to allow repeations (only when strictly nec.)
  // - Responsive/non-responsive simulator
  pure val honestParticipants = Set(1,2,3)
  pure val corruptedParticipants = Set(4,5,6)
  pure val allParticipants = honestParticipants.union(corruptedParticipants)
  pure val allVKs = 1.to(100)
  pure val docs = Set("doc1", "doc2", "doc3", "doc4", "doc5")


  var counter : int

  // Auxiliary types
  type Role = SigIF | SigSim | Env 

  type Participant = int
  type PublicKey = int
  type Document = str
  type Signature = int

  type History = {
    msg_log: Set[Message],
  }  

  // Mandatory types for the Choreo
  type Node = int
  type Message =

    | GenRequest(Participant) 
    | GenResponse((Participant, PublicKey))
    | SIMGen(Participant)
    | SIMReleaseGen((Participant, PublicKey))

    | SignRequest((Participant, Document))
    | SignResponse((Participant,(PublicKey, Document, Signature, bool)))
    | SIMSign((Participant, Document))  
    | SIMReleaseSign((Participant,Document,Signature))

    | VerifyRequest((Participant, PublicKey, Document, Signature))
    | VerifyResponse((Participant, (PublicKey, Document, Signature,bool)))
    | SIMVerify((Participant, PublicKey, Document, Signature))
    | SIMReleaseVerify(((Participant, PublicKey, Document, Signature), bool))


  type StateFields = {
    VK: Set[(Participant, PublicKey)],
    Ver: Set[(PublicKey, str, Signature, bool)],
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
        
  // --- Participant transitions ----------------------------------------------

  pure def vk_generation_ideal1(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role != SigIF){
      Set()
    }else
      ctx.messages.filterMap(m => {
        match m {
          | GenRequest(participant) => {
            if (ctx.state.VK.exists(x => x._1 == participant)){
              // finds the first one, also possible to traverse and generate multiple transitions
              val pk = ctx.state.VK.find(x => x._1 == participant).unwrap()._2

              Some({
                effects: Set( 
                    choreo::Send( ({ to: ENV, message: GenResponse((participant, pk)) })),
                    choreo::CustomEffect(ExcludeMsg((ctx.state.process_id, m)))
                            ),
                post_state: { ...ctx.state}
                   })
          // VK does not exist, send msg to SIM (no state to preserve)
          }else{
            Some({
              effects: Set( 
                choreo::Send(({ to: SIM, message: SIMGen(participant)})),
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
            1.to(5).map(vk =>
              Set({
                effects: Set( 
                    choreo::Send( ({ to: FSig, message: SIMReleaseGen((p, vk)) })),
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
        ctx.state.VK.exists(x => x._2 == p),
        ctx.state.Ver.exists(x => x._1 == p)
      }

    if(ctx.state.role != SigIF){
      Set()
    }else 
      ctx.messages.map(m => {
        match m {
          | SIMReleaseGen(p) => {
            {
              val participant = p._1
              val vk = p._2

              val san_vk = if(not (filter_fun(vk))){
                vk
              }else{
                1.to(100).find(p => not(filter_fun(p))).unwrap()
              }          

          Set({
            effects: Set( 
              
                choreo::Send( ({ to: ENV, message: GenResponse((participant, san_vk)) } ) ),
                choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m) ) )
            ),
            post_state: { ...ctx.state, VK: ctx.state.VK.union(Set((participant, san_vk))),}
          })
          }
        }
        // TODO: report bug
      | GenResponse(p) => Set()
      | SIMGen(p) => Set()
      | GenRequest(p) => Set()
      | SignRequest(p) => Set()
      | VerifyRequest(p) => Set()
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
            val participant = p._1
            val msg = p._2
            if(not (ctx.state.VK.exists(x => x._1 == participant))){
              Set()
            }else{
              Set({
                effects: Set( 
                    choreo::Send( ({ to: SIM, message: SIMSign((participant, msg))})),
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
            val participant = p._1
            val msg = p._2
            1.to(5).map(sig => 
              Set({
                effects: Set( 
                    choreo::Send( ({ to: FSig, message: SIMReleaseSign((participant, msg, sig))})),
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
            val participant = p._1
            val msg = p._2
            val sig = p._3
            val vk = ctx.state.VK.find(x => x._1 == participant).unwrap()._2

            def filter_fun = (x) => 
              ctx.state.Ver.exists(y => y._1 == vk and
                                        y._2 == msg and
                                        y._3 == x)
            

            val san_sig = if (filter_fun(sig)){
              1.to(100).find(x => not(filter_fun(x))).unwrap()
            }else{
              sig
            }
            Set({
              effects: Set( 
                  choreo::Send( ({ to: ENV, message: SignResponse((participant, (vk, msg, san_sig, true))) } ) ),
                  choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m) ) )
              ),
              post_state: { ...ctx.state, Ver: ctx.state.Ver.union(Set((vk, msg, san_sig, true))),}
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
            val participant = p._1
            val vk = p._2
            val msg = p._3
            val sig = p._4
            if(not (ctx.state.Ver.exists(x => x._1 == participant and x._2 == msg and x._3 == sig))){
              Set() // todo: send response
            }else{
              Set({
                effects: Set( 
                    choreo::Send( ({ to: SIM, message: SIMVerify((participant, vk, msg, sig))})),
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
                    choreo::Send( ({ to: FSig, message: SIMReleaseVerify((p,res))})),
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
            val p = q._1
            val participant = p._1
            val vk = p._2
            val msg = p._3
            val sig = p._4
            val res = q._2

            val san_res = if(ctx.state.VK.exists(x => x._1.in(honestParticipants) and x._2 == vk)){
              false
            }else{
              res
            }

            Set({ 
              effects: Set( 
                  choreo::Send( ({ to: ENV, message: VerifyResponse((participant, (vk,msg,sig,san_res)) ) } ) ),
                  choreo::CustomEffect(ExcludeMsg( (ctx.state.process_id, m) ) )
              ),
              post_state: { ...ctx.state, Ver: ctx.state.Ver.union(Set((vk, msg, sig, san_res))),}
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
    }else{ // VerifyRequest((Participant, PublicKey, Document, Signature))
      corruptedParticipants.map(participant => {
        1.to(5).map(vk => {
          docs.map(msg => {
            1.to(5).map(sig => {
            Set(
              { 
                effects: Set(
                  choreo::Send( ({ to: FSig, message: VerifyRequest((participant,vk,msg,sig) ) } ) )
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
      | ExcludeMsg(p) => { ... env, messages: env.messages.setBy(p._1, (old => old.exclude(Set(p._2)))) }
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
    all { gen_step3,counter' = counter,},

    all { sign_step1,counter' = counter,},
    all { sign_step2, counter' = counter,},
    all { sign_step3,counter' = counter,},

    all { verify_step1, counter' = counter,},
    all { verify_step2,counter' = counter,},
    all {
    counter' = counter,
    verify_step3,
    },

    all { counter < 1,
          env_step1 ,
          counter' = counter + 1

    }
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
       Set( GenRequest(4), 
            GenRequest(4), 
            GenRequest(3), 
            GenRequest(7), 
            SignRequest((4,"doc1")),
            SignRequest((3,"doc2")),
            SignRequest((4,"doc1")),
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
  
  // Property: participant has at most one verification key
  val OneVKperUser = allParticipants.forall(p => choreo::s.system.get(FSig).VK.filter(x => x._1 == p).size() <= 1)

  // Property: Ver is consistent (FAILS, flip-flop is possible because of verify functionality)
  val VerConsistent = choreo::s.system.get(FSig).Ver.forall(entry => {
    val q = if (entry._4) false else true
    val entry2 = (entry._1, entry._2, entry._3, q)
     not (entry2.in(choreo::s.system.get(FSig).Ver))
  })

  // Property: verification keys are unique 
  val VKUnique = allVKs.forall(p => choreo::s.system.get(FSig).VK.filter(x => x._2 == p).size() <= 1)

  // Property(Fails): If Ver containts some vk entry  then VK table must  contain respective entry
  val VKVerConsistentDishonest = choreo::s.system.get(FSig).Ver.forall(entry => {
    choreo::s.system.get(FSig).VK.exists(p => p._2 == entry._1) 
  })


  // Propery: If Ver contains entry then environment received a respective response (true for honest, fails for dishonest)
  val VerifiesIfSignedHonest = choreo::s.system.get(FSig).Ver.forall(entry => {
      if(entry._4){
        val vkentry = choreo::s.system.get(FSig).VK.find((x) => x._2 == entry._1)
        if(vkentry == None){
          true
        }else{
          val participant = vkentry.unwrap()._1

            // FAILS for corrupted participants 
            // choreo::s.messages.get(ENV).exists((m) => {
            //   m == SignResponse((participant, entry))
            // }) 

          if(participant.in(honestParticipants)){
            choreo::s.messages.get(ENV).exists((m) => {
              m == SignResponse((participant, entry))
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
    | SignResponse(p) => choreo::s.system.get(FSig).Ver.exists(x => x == p._2)
    | _ => true
  })
  // Property: if VerifyResponse is received then Ver contains the respective entry
  val VerifyResponseValid = choreo::s.messages.get(ENV).forall(m => match m {
    | VerifyResponse(p) => choreo::s.system.get(FSig).Ver.exists(x => x == p._2)
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