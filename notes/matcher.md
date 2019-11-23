# Schema Matcher Notes

## Heuristics

### Temporal info

- [implemented] total duration of notes
- [didn't work] total metric weight
- [implemented] stage spread (IOI between first and last note in stage)

### Rhythmic similarity

- note properties
    - onset
    - metric position
    - metric weight
    - duration ($\times$ onset)
- relation properties between notes
    - distance between notes of the same voice
    - distance between notes of the same event
- relation properties between events
- options
    - absolute vs relative encoding
    - notes in stage: voice order vs temporal order
- distance measures
    - euclidean
    - beat: cyclic euclidean
- comparison
    - all pairs of stages
      ```
      s1 s2 s3 s4
      => (s1,s2) (s1,s3) (s1,s4) (s2,s3) (s2,s4) (s3,s4)
      ```
    - stage transitions
      ```
      s1 s2 s3 s4
      => (s1->s2, s2->s3) (s1->s2, s3->s4) (s2->s3,s3->s4)
      ```
      
### Pitch

- [given] interval (class) pattern
- [implemented] octave displacement within voices
- average voice distance
- questions
    - doublings: pick inner or outer notes?

### Extensions

- Harmonic signature
    - simple harm. estimation on matched stages

## Model Structure

- current idea
    - logistic regression
    - binary classifier
    - pro:
        - interpretable
        - simple
        - few parameters
    - cons:
        - only works if features are very good
            - i.e. linear separation of classes

- probabilistic model
    - interpolation of sub-model
    - pro:
        - interpretable
        - can use smoothing
        - can use feature selections
        - doesn't require too much data
    - cons:
        - can require quite a bit of tinkering
        - difficult to model non-linearities

- feature combination
    - like viewpoints
    - like PULSE
    - simple feed-forward ANN