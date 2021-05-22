# Online Estimation of Surrogate Measures and its Application to Transportation Safety Analysis

## Background 

Traffic crashes are rare events, with an average rate of one crash 6.8 per million vehicle miles traveled in the US.
The naturalistic driving study (NDS) provides an unprecedented opportunity to **evaluate crash risk**.

We use data from Second Strategic Highway Research Program (SHRP2) NDS, the largest NDS to-date, with more than 3,400 participants
and 1 million hours of continuous driving data. The entire dataset comprised of about 2000 traffic crashes about 8000 near crashes, and tons of thousands of normal
driving segments.


## Data

The data comes from SHRP2 with noise added to protect the original ones for data privacy.

- Trainging Data: Each driving segment is about 200 time points, 1000 no risk segments and 400 risky segments.
- Testing Data: Each driving trip is about 2000 time points, 500 no risk trip and 200 risky trips.

## Aim of work

Our core concern is to propose surrogates based on three-dimension of acceleration.

1. Propose new surrogate measures with large power in detecting crash
2. Implement the surrogate measures with online version

## Project contents

- [Data](https://github.com/codeandworld/OnlineSurrogate/tree/main/data)
- [Report](https://github.com/codeandworld/OnlineSurrogate/blob/main/FinalReport.pdf)
- [Slides](https://github.com/codeandworld/OnlineSurrogate/blob/main/slides/onlinetraffic.pdf)
- [Rmarkdown](https://github.com/codeandworld/OnlineSurrogate/blob/main/code/OnlineSurrogate.Rmd)
