# Sandbox for Kadena REPL

## Introduction

This package helps to create a "Plug & Play" sandbox for easy development of Pact Smart Contracts off-chain, with all standards contracts preloaded and ready.

The goal is to simulate the Kadena environment as it can be found on-chain:

 * Namespaces
 * Coin-contract and fungible-v2 interfaces
 * Official Kadena utilities
 * Third party utilities
 * Marmalade interfaces and contracts

## How-to

1. Make sure that the Pact interpreter in installed.
  - https://github.com/kadena-io/pact/releases
2. Create your REPL file which:
  - load the main sandbox script `kda-env/init.repl`: `(env-data {})(load "kda-env/init.repl")`
  - declare or load and test your contracts
3. Optionally, you can disable some features by setting user data before loading `init.repl`
4. Launch your REPL file with the pact executable.

An example can be found: `example.repl` and `example.pact` and can be launched:
```
$ pact example.repl
Standard namespaces initialized
Kadena contracts initialized
Utils library contracts initialized
Marmalade contracts initialized
Hello bob: You have currently 1000.0 KDA
Load successful
```

## Description

### Pact
Based on Pact 4.7.0, and currently on-chains contracts (06/05/2023)

### Bootstrap modules

**basic-guards**: Implements two simple guards (used to initialize the namespaces):
 GUARD_SUCCESS and GUARD_FAILURE

**repl-coin-tools**: Implements 2 functions to create and mint coin accounts.`(env-enable-repl-natives)` must be enabled to make this module work.

### Namespaces
The following namespaces are created:
  - util
  - free
  - kip
  - user
  - marmalade

There is no enabled policy, and the module `ns` is not loaded.
There is no guard on the namespaces.

### Fungible-v2 and coin
The coin contract v5 is installed.

The followings accounts are pre-funded for test purpose:

| Account | Key       |  Balance |
|---------|-----------|----------|
| alice   | alice-key | 1000.0   |
| bob     | bob-key   | 1000.0   |
| carol   | carol-key | 1000.0   |
| dave    | dave-key  | 1000.0   |

If you need more accounts, you can create a new ones with `(repl-coin-tools.fund-account name key-name balance)`


### Marmalade
The version RC1 of marmalade is installed:
- https://github.com/kadena-io/marmalade/releases/tag/marmalade-rc1

Contracts:
- poly-fungible-v2 and token-policy-v1 interfaces in NS `kip`
- manifest contract in NS `kip`
- ledger in NS `marmalade`
- 3 default policy contracts in NS `marmalade`


### Pact Util library
The version 0.7 of unofficial Pact Util library:
- https://github.com/CryptoPascal31/pact-util-lib
- https://pact-util-lib.readthedocs.io/en/beta_0.7/

### Disabling features (optional)
If you don't need to load some features, you can disable them.
Before loading `init.repl` just use an `(env-data {...})` command with the following parameters:

  - `disable-util-lib` => Do not load the Pact Util Library modules
  - `disable-marmalade` => Do not load Marmalade modules
  - `disable-test-accounts` => Do not fund test accounts (Alice, Bob, ...)

  Examples:
  ```lisp
  ; Do not fund test account
  (env-data {"disable-test-accounts":true})
  (load "kda-env/init.repl")
  ```

  ```lisp
  ; Do not fund test account and do not load Marmalade
  (env-data {"disable-test-accounts":true,
             "disable-marmalade":true})
  (load "kda-env/init.repl")
  ```
