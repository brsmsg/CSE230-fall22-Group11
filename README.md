# CSE 230 Project - NS-Shaft Game
## Group Member
Yirun Zhao, Baihan Kang, Charlotte Tang, Wenshu Wu

## Proposal
### Description 

The player tries to enter a deep cave but must make it by landing on platforms. These platforms move upwards. At the top of the screen there are a bunch of spikes. Players have two ways to die: hitting too many spikes and running out of health, or falling from the bottom of the screen.

### Goal

The goal of the game is to survive as long as possible. Players must make sure they move fast to avoid getting hurt by top spikes, and also slow enough to not fall off the bottom of the screen.

We have also introduced a score system. The player will earn points and a certain health bonus  when stepping on a platform. You can also try to score as many points as possible.

### Platform design
There are several different platforms:
* **Normal platform**
* **Spike platform** : The player will get hurt when steps on it.
* **Temporary platform** : It disappears after the player steps on it.
* **Conveyor belt platform** : The player is forced to move to the right or to the left.
* **Spring platform** : The player will be bounced up when steps on it

## Updates (TODO)

Architecture of application
Challenges so far
Expect to meet goals?

## Demonstration (TODO)


## Timeline
* 11/09 Proposal
* 11/23 Update
* 12/07 Demonstration

## Setup The Game
```
  stack setup
  stack build
  stack exec ns-shaft
```