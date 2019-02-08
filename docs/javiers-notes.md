# General info
* Block mining target time is 20 seconds
* Confirmation is 2160 blocks (12 hours)

# On smart contracts

## Rollbacks

* Dependent transactions -> can be created immediately, a rollback will erase them too
* Other cases -> Check for 2160 confirmation blocks or whatever the application demands

# Scripts
https://github.com/input-output-hk/cardano-sl/blob/2817a37d95a71ec8380d82543d24dd94a22472a3/core/src/Pos/Core/Common/Script.hs
