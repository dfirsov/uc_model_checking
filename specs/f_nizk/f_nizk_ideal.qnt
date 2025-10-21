// -*- mode: Bluespec; -*-
module f_nizk_ideal {

  import basicSpells.* from "../../spells/basicSpells"
  import choreo(processes = NODES) as choreo from "../../choreo"

  pure val FNIZK : Node = 1
  pure val ENV   : Node = 2
  pure val SIM   : Node = 3
  pure val NODES  = Set(FNIZK,ENV,SIM)

  // - Disambiguate messages with salt(e.g., counter) to allow repeations (only when strictly nec.)
  // - Responsive/non-responsive simulator (idea: add some combinators for SIM)

  pure val proverParties = Set(1)
  pure val verifierParties = Set(2)
  pure val allParties = proverParties.union(verifierParties)
  pure val allStatements  = 1.to(5)
  pure val allWitnesses = 1.to(5)
  pure val allProofs = 1.to(5)




  // Auxiliary types
  type Role = NIZK_IF | SigSim | Env 

  type Party = int
  type Witness = int
  type Statement = int
  type Proof = int

  type History = {
    msg_log: Set[Message],
  }  

  // Mandatory types for the Choreo


  type Node = int
  type Message =

    | ProveRequest({party:Party, stm : Statement, witness: Witness}) // input for FSig
    | SIMProve({party:Party, stm : Statement, witness: Witness}) // input to SIM
    | SIMReleaseProve({party:Party, stm: Statement, witness : Witness, proof : Proof}) // output of SIM    
    | ProveResponse({proof : Proof}) // output of FSig

    | VerifyRequest({party:Party, stm: Statement, proof: Proof}) // input for FSig
    | SIMVerify({party:Party, stm : Statement, proof: Proof}) // input to SIM
    | SIMReleaseVerify({party:Party, stm: Statement, proof : Proof}) // output of SIM  
    | VerifyResponse(bool) // output of FSig

   

  type StateFields = {
    Ver:  Set[(Statement, Proof, bool)],
    Rel:  Set[(Statement, Witness)],
    role: Role,
  }

  type CustomEffects = ExcludeMsg({node:Node, msg:Message}) | Log(Message)
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


  pure def get_prove_request_messages(messages: Set[Message]): Set[{party:Party, stm : Statement, witness: Witness}] = {
    messages.filterMap(m => {
      match m {
        | ProveRequest(p) => Some(p)
        | _ => None
      }
    })
  }

  pure def get_prove_sim_request_messages(messages: Set[Message]): Set[{party:Party, stm : Statement, witness: Witness}] = {
    messages.filterMap(m => {
      match m {
        | SIMProve(p) => Some(p)
        | _ => None
      }
    })
  }  

  pure def get_prove_sim_release_request_messages(messages: Set[Message]): Set[{party:Party, stm: Statement, witness : Witness, proof : Proof}] = {
    messages.filterMap(m => {
      match m {
        | SIMReleaseProve(p) => Some(p)
        | _ => None
      }
    })
  }    


  /* End of boilerplate */
        
  // --- Party transitions ----------------------------------------------
  pure def prove_1(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role != NIZK_IF){
      ctx.messages.get_prove_request_messages().map(p => {
        val R = ctx.state.Rel
        if(not (R.contains((p.stm, p.witness)))){
          Set({
            effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: ProveRequest(p)}) )),
            post_state: { ...ctx.state }
          })
        }else{
          Set({
            effects: Set(
              choreo::Send(({ to: SIM, message: SIMProve(p) })),
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: ProveRequest(p)}) )),
            post_state: { ...ctx.state }
          })
        }
      }).flatten()
    }else{
      Set()
    }
  }

  pure def prove_sim(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role != SigSim){
      ctx.messages.get_prove_sim_request_messages().map(p => {
        allProofs.map(prf => {
          Set({
            effects: Set(
              choreo::Send(({ to: FNIZK, message: SIMReleaseProve({party:p.party, stm: p.stm, witness: p.witness, proof: prf}) })),
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SIMProve(p)}) )),
            post_state: { ...ctx.state }
          })
        }).flatten()
      }).flatten()
    }else{
      Set()
    }
  }      

  pure def prove_2(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role != NIZK_IF){
      ctx.messages.get_prove_sim_release_request_messages().map(p => {
      val san_pk =  if(ctx.state.Ver.contains((p.stm, p.proof, false))){
          allProofs.find(prf => not (ctx.state.Ver.contains((p.stm, prf, false)))).unwrap()
        }else{
          p.proof
        }

      Set({
        effects: Set(
          choreo::Send(({ to: p.party, message: ProveResponse({proof: san_pk}) })),
          choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SIMReleaseProve(p)}) )),
        post_state: { ...ctx.state, Ver: ctx.state.Ver.union(Set((p.stm, san_pk, true))) }
      })

        
       }).flatten()
    }else{
      Set()
    }
  }
  
      

  pure def main_listener(ctx: LocalContext): Set[Transition] =
    Set(
      prove_1(ctx),
      prove_sim(ctx),
      prove_2(ctx) 
    ).flatten()

 def apply_custom_effect(env: GlobalContext, effect: CustomEffects): GlobalContext = {
    match effect {
      | ExcludeMsg(p) => {... env, messages: env.messages.setBy(p.node, (old => old.exclude(Set(p.msg)))) }
      | Log(msg) => {
          {...env, extensions: {...env.extensions, msg_log: env.extensions.msg_log.setAdd(msg)}}
        }
    }
  }

  action step = choreo::step(
    (ctx => main_listener(ctx)),
    apply_custom_effect
  )

  pure def initialize(n: Node): LocalState = {
    {
      process_id: n,
      Rel : Set(),
      Ver: Set(),
      role: if (n == FNIZK) NIZK_IF else if (n == SIM) SigSim else Env
    }
  }

  pure val initial_history = {
    msg_log: Set(),
  }  

  action init = all {
    choreo::init({
      system: NODES.mapBy(n => initialize(n)),
      messages: NODES.mapBy(n => if (n == 1)  
         Set() 
       else Set()),
    events: NODES.mapBy(n => Set()),
    extensions: initial_history
  })  ,
  }  
}