# CSE 230 Project - NS-Shaft Game
## Group Member
Yirun Zhao, Baihan Kang, Charlotte Tang, Wenshu Wu

## Proposal
### Description 

The player tries to enter a deep cave but must make it by landing on platforms. These platforms move upwards. Players have two ways to die: hitting too many spikes and running out of health, or falling from the bottom of the screen.

### Goal

The goal of the game is to survive as long as possible. Players must make sure they move fast to avoid getting hurt by top spikes, and also slow enough to not fall off the bottom of the screen.

We have also introduced a score system. The player will earn points and a certain health bonus when stepping on a platform. You can also try to score as many points as possible.

### Platform design
There are several different platforms:
* **Normal platform**
* **Spike platform** : The player will get hurt when steps on it.
* **Conveyor belt platform** : The player is forced to move to the right or to the left.
* **Heal platform** : The player will be healed when steps on it

## Updates

#### Architecture of application:
We organized our project in model-view-controller(MVC) architecture, enlightened by the Sam Tay Snake Game tutorial: https://github.com/samtay/snake
The directory tree of our project: 
```
.
├── CHANGELOG.md
├── CSE230-fall22-Group11.cabal
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── package.yaml
├── src
│   ├── Controller.hs
│   ├── Lib.hs
│   ├── Shaft.hs
│   └── UI.hs
├── stack.yaml
├── stack.yaml.lock
└── test
    └── Spec.hs

```
Shaft.hs is the model layer containing the basic type definitions of data structures in our project, like the basic definition of States and Events in Brick and different types of platforms. UI.hs is the view layer that implements the user interface using Brick. Controller.hs declared basic control logic functions to concatenate models and views. Lib.hs includes some common tool functions.

#### Challenges so far
1. Merge different parts from all team members and ensure the project works well.
2. It takes time to learn to design and implement some complex features 
    * Randomly generated platforms.
    * The interaction of characters and different types of platforms, eg. spring platforms make the character spring up.
    * The acceleration of platforms over time.
3. It takes time to figure out how to generalize the definition of platforms, i.e. different platforms can share the same code to reduce redundancy.


#### Expect to meet goals?
We expect to meet our goal before the deadline


## Demonstration (TODO)


## Timeline
* 11/09 Proposal
* 11/23 Update
* 12/07 Demonstration

## Setup The Game
```
git clone https://github.com/brsmsg/CSE230-fall22-Group11.git`
cd CSE230-fall22-Group11
stack run
```

or you can use:

```
  stack setup
  stack build
  stack exec ns-shaft
```