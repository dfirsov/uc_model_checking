module f_ac_ideal {

  import basicSpells.* from "../../spells/basicSpells"
  import choreo(processes = NODES) as choreo from "../../choreo"      
  import g_clock_ideal as g_clock from "../g_clock/g_clock_ideal" 

  // paramters: TODO Parameters have to be moved into functionality itself. 
  pure val delta = 1
  pure val sid = 1

  pure val Q = Set(1,2,3)
  pure val P = Set(1,2) // parties
  pure val C = Set(2)
  
  pure val FAC   : Node = 1
  pure val ENV   : Node = 2
  pure val SIM   : Node = 3
  pure val NODES  = Set(FAC,ENV,SIM)

  type Role = FAC_ROLE | SIM_ROLE | ENV_ROLE 

  type History = {
    msg_log: Set[({idx:int, msg: Message})],
  }  

  // Mandatory types for the Choreo
  type Node = int
  type Party = int
  type Data = int
  type Time = int
  type Counter = int

  type Message =

    | SendRequest({from:Party, to:Party, data:Data}) 
    | SIMSendHold({from:Party, to:Party, ctr:Counter, time:Time, data:Data})
    | SIMSendRelease({from:Party, to:Party, time:Time, ctr:Counter})
    | SendResponse

    | FetchRequest(Party)
    | SIMFetchHold({time:Time, i_late: Set[Time], party:Party})
    | SIMFetchRelease({time:Time, i_late: Set[Time], i_early: Set[Time], party:Party})
    | FetchResponse({to: Party, data: Set[Data]})

    | OKRequest(Party)
    | ClockRequest({party:Party, time:Time})
    | ClockRequestWait({party:Party, time:Time}) // wait until Clock will finish its business
    | ClockResponse({party:Party, time:Time})
    | OKResponse

    | EnvGiveControl
    | SIMGiveControl  

  
  type StateFields = {
    // functionality variables
    ctr: Counter,
    L  : Counter -> {from: Party, to:Party, time:Time, data:Data},
    done : {from:Party,to:Party,time:Time} -> bool,
    role : Role,

    // simulator variables
    sim_handles : Set[(Message)], // for SIM to store hold requests
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
  
  pure def get_ok_request_messages(messages: Set[Message]): Set[Party] = {
    messages.filterMap(m => {
      match m {
        | OKRequest(p) => Some(p)
        | _ => None
      }
    })
  }
    
  pure def get_ok_response_messages(messages: Set[Message]): Set[()] = {
    messages.filterMap(m => {
      match m {
        | OKResponse => Some(())
        | _ => None
      }
    })
  }      

  pure def get_send_request_messages(messages: Set[Message]): Set[{from:Party, to:Party, data:Data}] = {
    messages.filterMap(m => {
      match m {
        | SendRequest(p) => Some(p)
        | _ => None
      }
    })
  }

  pure def get_sim_send_hold_messages(messages: Set[Message]): Set[{from:Party, to:Party, ctr:int, time:Time, data:Data}] = {
    messages.filterMap(m => {
      match m {
        | SIMSendHold(p) => Some(p)
        | _ => None
      }
    })
  }

  pure def get_sim_send_release_messages(messages: Set[Message]): Set[{from:Party, to:Party, time:Time, ctr:int}] = {
    messages.filterMap(m => {
      match m {
        | SIMSendRelease(p) => Some(p)
        | _ => None
      }
    })
  }

  pure def get_clock_request_messages(messages: Set[Message]): Set[{party:Party, time:Time}] = {
    messages.filterMap(m => {
      match m {
        | ClockRequest(p) => Some(p)
        | _ => None
      }
    })
  }

  pure def get_clock_request_wait_messages(messages: Set[Message]): Set[{party:Party, time:Time}] = {
    messages.filterMap(m => {
      match m {
        | ClockRequestWait(p) => Some(p)
        | _ => None
      }
    })
  }  

  pure def get_clock_response_messages(messages: Set[Message]): Set[{party:Party, time:Time}] = {
    messages.filterMap(m => {
      match m {
        | ClockResponse(p) => Some(p)
        | _ => None
      }
    })
  }  

  pure def get_send_response_messages(messages: Set[Message]): Set[()] = {
    messages.filterMap(m => {
      match m {
        | SendResponse => Some(())
        | _ => None
      }
    })
  }

  pure def get_fetch_request_messages(messages: Set[Message]): Set[Party] = {
    messages.filterMap(m => {
      match m {
        | FetchRequest(p) => Some(p)
        | _ => None
      }
    })
  }

  pure def get_sim_fetch_hold_messages(messages: Set[Message]): Set[{time:Time, i_late: Set[Time], party:Party}] = {
    messages.filterMap(m => {
      match m {
        | SIMFetchHold(p) => Some(p)
        | _ => None
      }
    })
  }

  pure def get_sim_fetch_release_messages(messages: Set[Message]): Set[{time:Time, i_late: Set[Time], i_early: Set[Time], party:Party}] = {
    messages.filterMap(m => {
      match m {
        | SIMFetchRelease(p) => Some(p)
        | _ => None
      }
    })
  }

  pure def get_fetch_response_messages(messages: Set[Message]): Set[{to: Party, data: Set[Data]}] = {
    messages.filterMap(m => {
      match m {
        | FetchResponse(p) => Some(p)
        | _ => None
      }
    })
  }

  pure def get_env_give_control_messages(messages: Set[Message]): Set[()] = {
    messages.filterMap(m => {
      match m {
        | EnvGiveControl => Some(())
        | _ => None
      }
    })
  }

  pure def get_sim_give_control_messages(messages: Set[Message]): Set[()] = {
    messages.filterMap(m => {
      match m {
        | SIMGiveControl => Some(())
        | _ => None
      }
    })
  }
  
  def send_1(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == FAC_ROLE){
      ctx.messages.get_send_request_messages().map(p => {

        if(Q.contains(p.to)){
          val t = g_clock::choreo_s.system.get(g_clock::GLOCK).TIME // to make traces shorter
          val new_ctr = ctx.state.ctr + 1
          val new_done = ctx.state.done.put({from:p.from,to:p.to,time:t}, false)
          val new_L = ctx.state.L.put(new_ctr, {from:p.from, to:p.to, time:t, data:p.data})
           {
              effects: Set(
                choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SendRequest(p)}) ),
                choreo::CustomEffect(Log(SendRequest(p))),

                choreo::Send(({to: SIM, message: SIMSendHold({
                  from:p.from,
                  to:p.to,
                  ctr:new_ctr,
                  time:t,
                  data:p.data
                })})),
              ),

              post_state: { ...ctx.state, 
                done: new_done,
                ctr: new_ctr,
                L: new_L
              }, 
          }
        }else{
           {
              effects: Set(
                choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SendRequest(p)}) ),
                choreo::CustomEffect(Log(SendRequest(p))),
                choreo::Send({to: ENV, message: SendResponse}), // TODO: indicate failure?
              ),
              post_state: ctx.state, 
          }
        }
      })
    }else{
      Set()
    }
  }  

  pure def sim_send_hold_reqs(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == SIM_ROLE and ctx.messages.get_sim_send_hold_messages().size() > 0){
      val p = ctx.messages.get_sim_send_hold_messages().find(x => true).unwrap()
      Set({
            effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SIMSendHold(p) })),
              choreo::Send({ to: SIM, message: SIMGiveControl }),
            ),
            post_state: { ...ctx.state, 
              sim_handles: ctx.state.sim_handles.union(Set(SIMSendRelease({
                from: p.from,
                to:   p.to,
                time: p.time,
                ctr:  p.ctr
              }))),
            }, 
          })
    }else{
      Set()
    }
  }    

  pure def send_2(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == FAC_ROLE){
      ctx.messages.get_sim_send_release_messages().map(p => {
        {
            effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SIMSendRelease(p)}) ),
              choreo::Send(({to: ENV, message: SendResponse})),
            ),
            post_state: { ...ctx.state, 
              //done: ctx.state.done.set({from:p.from, to:p.to, time:p.time}, true),
              done: ctx.state.done.put({from:p.from, to:p.to, time:p.time}, true), // TODO: above must be correct
            }, 
        }
      })
    }else{
      Set()
    }
  }    

  pure def env_fetch_response(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == ENV_ROLE){
      ctx.messages.get_fetch_response_messages().map(p => {
        {
            effects: Set(
              choreo::Send(({to: ENV, message: EnvGiveControl})),
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: FetchResponse(p)}) ),
              choreo::CustomEffect(Log(FetchResponse(p))),
            ),
            post_state: ctx.state, 
        }
      })
    }else{
      Set()
    }
  }  


  pure def env_ok_response(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == ENV_ROLE){
      ctx.messages.get_ok_response_messages().map(p => {
        {
            effects: Set(
              choreo::Send(({to: ENV, message: EnvGiveControl})),
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: OKResponse}) ),
              choreo::CustomEffect(Log(OKResponse)),
            ),
            post_state: ctx.state, 
        }
      })
    }else{
      Set()
    }
  }        

  pure def sim_process_hold_reqs(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == SIM_ROLE and ctx.state.sim_handles.size() > 0 and ctx.messages.contains(SIMGiveControl)){
      val release_trans = ctx.state.sim_handles.map(release_request =>
          {
            effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SIMGiveControl })),
              choreo::Send({ to: FAC, message: release_request  }), 
            ),
            post_state: { ...ctx.state, 
              sim_handles: ctx.state.sim_handles.exclude(Set(release_request)),
            }, 
          }
      )

      release_trans.union(
        Set({
            effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SIMGiveControl })),
              choreo::Send(({ to: ENV, message: EnvGiveControl })),
            ),
            post_state: ctx.state, 
          })      
      )
    }else{
      Set()
    }
  }


  def fetch_1(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == FAC_ROLE){
      ctx.messages.get_fetch_request_messages().map(p => {

        val tp = g_clock::choreo_s.system.get(g_clock::GLOCK).TIME // to make traces shorter
        val i_late = ctx.state.L.keys().filterMap(k => {
          val v = ctx.state.L.get(k)
          if(v.to == p and (tp - v.time) == delta){
            Some(k)
          }else{
            None
          }
        })

        {
            effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: FetchRequest(p)}) ),
              choreo::CustomEffect(Log(FetchRequest(p))),
              choreo::Send(({to: SIM, message: SIMFetchHold({
                time: tp,
                i_late: i_late,
                party: p
              })})),              
            ),
            post_state: ctx.state
          }
      })
    }else{
      Set()
    }
  }  

  pure def sim_fetch_hold_reqs(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == SIM_ROLE and ctx.messages.get_sim_fetch_hold_messages().size() > 0){
      val p = ctx.messages.get_sim_fetch_hold_messages().find(x => true).unwrap()
      Set({
            effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SIMFetchHold(p) })),
              choreo::Send({ to: SIM, message: SIMGiveControl }),
            ),
            post_state: { ...ctx.state, 
              sim_handles: ctx.state.sim_handles.union(Set(SIMFetchRelease({
                time: p.time,
                i_late:  p.i_late,
                i_early: Set(), // TODO: add some nondeterministic choice here
                party: p.party
              }))),
            }, 
          })
    }else{
      Set()
    }
  }     

  def fetch_2(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == FAC_ROLE){
      ctx.messages.get_sim_fetch_release_messages().map(p => {
        val i_late = p.i_late
        // remove filter map to check model checker's liveness
        val i_early  = p.i_early.filterMap(i => {
          val v = ctx.state.L.get(i)
          if(v.to == p.party){
            Some(i)
          }else{
            None
          }
        })
        val msgs = i_early.union(i_late).map(k => ctx.state.L.get(k).data)
        val new_L = ctx.state.L.keys().exclude(i_early.union(i_late)).mapBy(k => ctx.state.L.get(k))

        {
            effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: SIMFetchRelease(p)})),
              choreo::Send(({to: ENV, message: FetchResponse({to: p.party,data:msgs})})),               
            ),
            post_state: { ...ctx.state, 
              L : new_L
            }, 
        }
      })
    }else{
      Set()
    }
  }    

  def ok_1(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == FAC_ROLE){
      ctx.messages.get_ok_request_messages().map(p => {
        val tp = g_clock::choreo_s.system.get(g_clock::GLOCK).TIME // to make traces shorter

        val allDone = P.forall(q => {
          val done = choreo::s.system.get(ENV).done
          val k = {from:p, to:q, time:tp}
          done.has(k).implies(done.get(k) == true)
        })

        if(allDone){
          Set({
            effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: OKRequest(p)}) ),
              choreo::CustomEffect(Log(OKRequest(p))),
              choreo::Send(({to: ENV, message: ClockRequest({party:p,time:tp})})),              
            ),
            post_state: { ...ctx.state, }, 
          })
        }else{
          Set()
        }
      }).flatten()
    }else{
      Set()
    }
  }  

  action ok_from_ac_to_clock : bool = all {
    choreo::s.messages.get(ENV).get_clock_request_messages().size() > 0,
    val p = choreo::s.messages.get(ENV).get_clock_request_messages().find(x => true).unwrap()

    val transition1 =
        Set({
          effects: Set(
            g_clock::choreo_Send(({ to: g_clock::GLOCK, message: g_clock::OkayRequest({party:p.party, sid: sid}) })),
          ),
          post_state: g_clock::choreo_s.system.get(g_clock::GLOCK), 
        })

    val transition2 =
        Set({
          effects: Set(
              choreo::CustomEffect(ExcludeMsg( {node:ENV, msg: ClockRequest(p)}) ),
              choreo::CustomEffect(Log(ClockRequest(p))),  
              choreo::Send(({ to: ENV, message: ClockRequestWait(p) })),             
          ),
          post_state: choreo::s.system.get(ENV), 
        })      

    all {
      g_clock::choreo_process_transitions(g_clock::GLOCK, transition1, g_clock::apply_custom_effect),
      choreo::process_transitions(ENV, transition2, apply_custom_effect),
    }
  }


  action ok_from_clock_to_ac : bool = all {
    g_clock::choreo_s.messages.get(g_clock::ENV).g_clock::get_okay_response_messages().size() > 0,

    val partysid = g_clock::choreo_s.messages.get(g_clock::ENV).g_clock::get_okay_response_messages().find(x => true).unwrap()
    val p = choreo::s.messages.get(ENV).get_clock_request_wait_messages().find(x => true).unwrap()
    // assert p partysid are the same parties

    val transition1 =
        Set({
          effects: Set(
            g_clock::choreo_CustomEffect(g_clock::ExcludeMsg( {node:g_clock::ENV, msg: g_clock::OkayResponse(partysid) }) ),
          ),
          post_state: g_clock::choreo_s.system.get(g_clock::ENV), 
        })

    val transition2 =
        Set({
          effects: Set(
              choreo::Send(({ to: FAC, message: ClockResponse(p) })),     
              choreo::CustomEffect(ExcludeMsg( {node:ENV, msg: ClockRequestWait(p)}) ),  
          ),
          post_state: choreo::s.system.get(ENV), 
        })
    
    all {
      g_clock::choreo_process_transitions(g_clock::ENV, transition1, g_clock::apply_custom_effect),
      choreo::process_transitions(ENV, transition2, apply_custom_effect),
    }    
  }


  def ok_2(ctx: LocalContext) : Set[Transition] = {
    if(ctx.state.role == FAC_ROLE){
      ctx.messages.get_clock_response_messages().map(p => {
        {
          effects: Set(
            choreo::CustomEffect(ExcludeMsg( {node:ctx.state.process_id, msg: ClockResponse(p)}) ),
            choreo::CustomEffect(Log(ClockResponse(p))),
            choreo::Send(({to: ENV, message: OKResponse})),              
           ),
          post_state: { ...ctx.state, 
            done: ctx.state.done.keys().exclude(
              ctx.state.done.keys().filterMap(k => 
                {
                  if(k.from == p.party and k.time == p.time){
                    None
                  }else{
                    Some(k)
                  }
                }
              )
            ).mapBy(k => ctx.state.done.get(k))

          }, 
        }
      })
    }else{
      Set()
    }
  }  


  // TODO: add extra effects
  //  1. SIM in GLOCK can give control to FAC.
  //  2. SIM in FAC can give control to GLOCK.
  //  3. ENV can also interact with g_clock but with restricted set of parties and session ids.

  def main_listener(ctx: LocalContext): Set[Transition] =
    Set(
      send_1(ctx),
      sim_send_hold_reqs(ctx),
      send_2(ctx),

      fetch_1(ctx),
      sim_fetch_hold_reqs(ctx),
      fetch_2(ctx),

      ok_1(ctx),
      ok_2(ctx),
      
      sim_process_hold_reqs(ctx),

      env_fetch_response(ctx),
      env_ok_response(ctx),
    ).flatten()


  pure def apply_custom_effect(env: GlobalContext, effect: CustomEffects): GlobalContext = {
    match effect {
      | ExcludeMsg(p) => {... env, messages: env.messages.setBy(p.node, (old => old.exclude(Set(p.msg))))}
      | Log(msg) => {
          {...env, extensions: {...env.extensions, msg_log: env.extensions.msg_log.setAdd(
            { idx:env.extensions.msg_log.size(),
              msg:msg,
            })}}
      }
    }
  }

pure def initialize(n: Node): LocalState = {
    {
      process_id: n,
      role: if (n == FAC) FAC_ROLE else if (n == SIM) SIM_ROLE else ENV_ROLE,
      L: Map(),
      done: Map(),
      ctr : 0,
      sim_handles : Set(),
    }
  }

  action stutter = {
    choreo::s' = choreo::s
  }

  pure val initial_history = {
    msg_log: Set(),
  }  

  action init = all {
    choreo::init({
      system: NODES.mapBy(n => initialize(n)),
      messages: NODES.mapBy(n => if (n == ENV)  
         Set(EnvGiveControl)
       else Set()),
      events: NODES.mapBy(n => Set()),
      extensions: initial_history
    }),
    // register all Q parties in g_clock
    g_clock::init_with_regs(P.map(p => {party: p, sid: sid}))
  }

  action step = any {
    // ac goes, g_clock stutters
    all {
      choreo::step((ctx => main_listener(ctx)),apply_custom_effect),
      g_clock::stutter
    },
    // g_clock goes, ac stutters
    all {
      stutter,
      g_clock::step
    },    
    // routing of messages from and to g_clock
    ok_from_ac_to_clock,
    ok_from_clock_to_ac,

    // environment actions
    all {
      env_act,
      g_clock::stutter
    }
  }  

  // define an action that non-determinically sends send, fetch, or ok messages to FAC
  action env_send_reqs : bool = {
    nondet to_party = P.oneOf()
    nondet data = 1.to(10).oneOf()

    val transition = {
          effects: Set(
            choreo::Send({ to: FAC, message: SendRequest({from: ENV, to: to_party, data: data}) }),
            choreo::CustomEffect(ExcludeMsg( {node:ENV, msg: EnvGiveControl} )),
          ),
          post_state: choreo::s.system.get(ENV),
        }
    
    choreo::process_transitions(ENV, Set(transition), apply_custom_effect)
  }

  action env_fetch_reqs : bool = {
    nondet from_party = P.oneOf()
    val transition = {
          effects: Set(
            choreo::Send({ to: FAC, message: FetchRequest(from_party) }),
            choreo::CustomEffect(ExcludeMsg( {node:ENV, msg: EnvGiveControl} )),
          ),
          post_state: choreo::s.system.get(ENV),
        }
    
    choreo::process_transitions(ENV, Set(transition), apply_custom_effect)
  }

  action env_ok_reqs : bool = {
    nondet from_party = P.oneOf()
    val transition = {
          effects: Set(
            choreo::Send({ to: FAC, message: OKRequest(from_party) }),
            choreo::CustomEffect(ExcludeMsg( {node:ENV, msg: EnvGiveControl} )),
          ),
          post_state: choreo::s.system.get(ENV),
        }
    
    choreo::process_transitions(ENV, Set(transition), apply_custom_effect)
  }

  action env_act = all {
    choreo::s.messages.get(ENV).contains(EnvGiveControl),
    any {
      env_send_reqs,
      env_fetch_reqs,
      env_ok_reqs,
    }
  }


// If some message was delivered from P to Q then P indeed sent that message to Q in the past.
val AuthenticationProperty = choreo::s.extensions.msg_log.forall(log_entry1 => {
  match log_entry1.msg {
    //| FetchRequest(p) => false
    | FetchResponse(p) => { // 
        // find corresponding FetchRequest in the log
        (p.data != Set()) implies choreo::s.extensions.msg_log.exists(log_entry2 => {
          match log_entry2.msg {
            | SendRequest(q) => {
                false
              }
            | _ => false
          }
        })
      }
    | _ => true
  }
})

val Prop = g_clock::choreo_s.system.get(g_clock::GLOCK).TIME <= 1
    
}