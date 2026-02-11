# F-AC: Authenticated Channel

## Overview

F-AC (Authenticated Channel) provides a formal specification of an idealized authenticated communication channel with guaranteed delivery delay. This module models a cryptographic functionality that allows parties to send authenticated messages to each other with a deterministic delivery delay (delta). Messages sent at time t are guaranteed to be delivered by time t + delta, with authentication ensuring that recipients know the authentic sender while preventing message duplication and early delivery.

## Module Structure

### Core Files

- **[f_ac_types.qnt](f_ac_types.qnt)**: Type definitions and data structures
  - `FAC_Message`: Union of all message types (SendRequest, FetchRequest, OKRequest, responses, and SIM controls)
  - `FAC_SystemParams`: System parameters including parties P, corrupted parties C, delivery delay delta, and SIDs
  - `FAC_StateVars`: Per-SID state containing message log L, counter, send flags S_flag, and fetch flags F_flag
  - `FAC_System`: Complete system state with parameters, per-SID state, input buffer, handles, and event log

- **[f_ac.qnt](f_ac.qnt)**: Core protocol implementation
  - Initialization: `fac_init`
  - Send: Three-phase send operation with simulator control (`fac_send_1`, `fac_send_2`, `fac_send_3`)
  - Fetch: Three-phase fetch operation with simulator control (`fac_fetch_1`, `fac_fetch_2`, `fac_fetch_3`)
  - OK: advancing the clock

- **[f_ac_env.qnt](f_ac_env.qnt)**: Environment specification
  - Test cases and environment interactions for the f_ac functionality

## Verified Properties [f_ac_properties.qnt](f_ac_properties.qnt)

- **Delta-Delayed Liveness (DDL)**: If P sends a message x to Q at time t, and Q attempts to fetch at time t' ≥ t + delta, then Q receives x in some round t'' ∈ [t, t']
- **Authentication** : If a nonempty set of messages was delivered from P to Q then P indeed sent that message to Q in the past.
- **No Duplication**: At time t, a message is delivered at most once to the same party

## Functionality

![f-ac](pic.png?raw=true)