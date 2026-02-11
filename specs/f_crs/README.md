# F-CRS: Common Reference String

## Overview

F-CRS (Common Reference String functionality) provides a formal specification of an idealized common reference string functionality. This module models a cryptographic functionality that allows parties to request and obtain a uniformly random common reference string (CRS) that is consistent across all parties for a given session identifier (SID). The CRS is generated once per SID and remains identical for all parties querying that SID, enabling the construction of non-interactive zero-knowledge proofs and other cryptographic protocols.

## Module Structure

### Core Files

- **[f_crs_types.qnt](f_crs_types.qnt)**: Type definitions and data structures
  - `CRS_Message`: Union of all message types (GenRequest, GenResponse, and SIM controls)
  - `CRS_SystemParams`: System parameters including parties P, available CRS values D, and SIDs
  - `CRS_StateVars`: Per-SID state containing the generated CRS value (Option[CRS])
  - `CRS_System`: Complete system state with parameters, per-SID state, input buffer, handles, nonce, and event log

- **[f_crs.qnt](f_crs.qnt)**: Core protocol implementation
  - Initialization: `crs_init`
  - Generation: Three-phase CRS generation with simulator control (`crs_gen_1`, `crs_gen_2`, `crs_gen_3`)
  - Simulator control: Manages the hold and release phases of CRS generation (`crs_sim_loses_ctrl`)

## Verified Properties [f_crs_properties.qnt](f_crs_properties.qnt)

- **CRS Consistency (CRS_same)**: For any two GenResponse messages in the log with the same SID, they return the same CRS value (p1.cfg.sid == p2.cfg.sid ⟹ p1.crs == p2.crs)
- **SID Differentiation**: Different SIDs may be assigned different CRS values (demonstrated as a co-property that allows variability across sessions)

## Functionality

![f-ac](pic.png?raw=true)