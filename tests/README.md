# Tests Scripts

Simple test script to verify that the local modules are the same than the on-chain deployed modules.

## On-chain

```
./run_on_chain.sh
```

Use `kda tool`, and `jq` to retrieve the hashes of deployed-module

## Off-chain
 ```
 ./run_off_chain.sh"
 ```

 Use the `pact` executable to retrieve the hashes of the modules of this package.


 ## Comparison

 Both lists can be compared to verify that everything is up-to-date respect to the blockchain.

**Note:**: For some reasons, `util.guards1` is expected to be different.
