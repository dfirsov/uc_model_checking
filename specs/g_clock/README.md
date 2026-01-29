# G-Clock: Global Clock

## Overview

G-Clock (Global Clock) provides a formal specification of a global clock functionality for distributed systems. This module models an ideal functionality that enables parties to synchronize on time, ensuring that all parties see the same current time value and that time advances only when all registered parties are ready. It provides a global notion of time for protocols that require temporal coordination.

## Module Structure

### Core Files

- **[g_clock_types.qnt](g_clock_types.qnt)**: Type definitions and data structures
  - `PartySID`: Pair of (party, SID) for registration tracking
  - `Time`: Time value type (integer)
  - `GC_Message`: Union of all message types (RegisterRequest, TimeRequest, OkayRequest, responses, SIM controls)
  - `GC_System`: System state containing input buffer, handles, state (TIME, Reg, ok sets), parameters, nonce, and event log
  - State: TIME (current time), Reg (registered parties), ok (parties ready to advance)

- **[g_clock.qnt](g_clock.qnt)**: Core protocol implementation
  - Initialization: `gc_init`
  - Register: One-phase registration (`gc_register`)
  - TimeRequest: Single operation to read current time (`gc_time`)
  - OkayRequest: Two-phase time advance with simulator control (`gc_okay_1`, `gc_okay_2`)
  - Main listener: `gc_main_listener` coordinates all operations

- **[g_clock_properties.qnt](g_clock_properties.qnt)**: Formal property specifications
  - Time agreement: Parties see same time if no advancement in between
  - Time liveness: Time advances when all registered parties are ready
  - Time soundness: Time only advances when all registered parties signal readiness
  - System properties: Input bounds and log growth

## Functionality

![g-clock](pic.png?raw=true)

## Verified Properties [g_clock_properties.qnt](g_clock_properties.qnt)
- **Time monotonicity**: Clock time advances monotonically
- **Registration tracking**: Parties must register before using clock
- **Synchronization**: All registered parties see consistent time values
- **Handle management**: Proper management of clock simulation holds