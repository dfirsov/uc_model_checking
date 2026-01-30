# F-DIF: Message Diffusion Protocol

## Overview

F-DIF provides a formal specification of an idealized message diffusion protocol for distributed systems. This module models an UC ideal functionality that enables a sender to broadcast messages to multiple recipients with guaranteed delivery after a specified delay (delta), while supporting adversarial injection from corrupted parties. It ensures that messages diffused at time t are available to all parties by time t + Δ.

## Module Structure

### Core Files

- **[f_dif_types.qnt](f_dif_types.qnt)**: Type definitions and data structures
  - `DIF_Data`: Message data with party, aggregate verification key (avk), and signature
  - `Counter`: Message sequence counter for tracking diffused messages
  - `DIF_Message`: Union of all message types (DiffuseRequest, FetchRequest, InjectRequest, responses, SIM controls)
  - `DIF_System`: System state containing input buffer, handles, state per SID, parameters, nonce, and event log
  - State per SID: Ledger L (message history), counter, no-reentrance flags (D_flag for diffusion, F_flag for fetch)

- **[f_dif.qnt](f_dif.qnt)**: Core protocol implementation
  - Initialization: `dif_init`
  - Diffuse: Three-phase broadcast with simulator control (`dif_diffuse_1`, `dif_diffuse_2`, `dif_diffuse_3`)
  - Fetch: Three-phase retrieval with time bounds (`dif_fetch_1`, `dif_fetch_2`, `dif_fetch_3`)
  - Inject: Corrupted party message insertion (`dif_inject`)
  - OK: Time advancement synchronization with G-Clock
  - Simulator control: Manages adversarial behavior

- **[f_dif_env.qnt](f_dif_env.qnt)**: Execution environment
  - init and step functions
  - Request creation and response consumption

- **[f_dif_properties.qnt](f_dif_properties.qnt)**: Formal property specifications
  - No duplication: Messages delivered at most once per time
  - Delta-delayed liveness: Messages available after delta delay
  - Authentication: Delivered messages were previously diffused
  - Diffusion consistency: Same data for all recipients
  - Inject access control: Only corrupted parties can inject

## Functionality

![f-dif](pic.png?raw=true)




