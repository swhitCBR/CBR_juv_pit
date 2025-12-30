
```mermaid
---

config:

  sequence:

    mirrorActors: false

  look: classic

  theme: default

---

sequenceDiagram

        participant Release

        participant LGR

        participant MCN

        participant BON

        participant Estuary

  
  
  

        Release->>LGR: $$S_{0}$$

        Note over LGR: $$P_{LGR}$$

        LGR->>MCN: $$S_{1}$$

        Note over MCN: $$P_{MCN}$$

        MCN->>BON: $$S_{2}$$

        Note over BON: $$P_{BON}$$

        Note left of Estuary: $$\lambda$$

        LGR->>BON: $$S_{1} \cdot S_{2}$$
```

```mermaid
flowchart LR

    R["R"] --> V
    V -- S --> P("p")
    P --> $$\lambda$$
```

```mermaid
---

config:

  sequence:

    mirrorActors: false

---

sequenceDiagram

        participant Release

        participant LGR

        participant MCN

        participant BON

        participant Estuary

  

        Release->>LGR: $$S_{0}$$

        Note over LGR: $$P_{0}$$

        LGR->>MCN: $$S_{0}$$

        Note over MCN: $$P_{0}$$

        MCN->>BON: $$S_{0}$$

        Note over BON: $$P_{0}$$

        Note left of Estuary: $$\lambda$$
```

```mermaid
flowchart LR

    a["Release"] --> b["Site 1"]

    b --> c["Site 2"]

    c --- d["Expected Counts"]

    R["$$R$$"] L_R_x11_0@--> x11["$$X$$<sub>11</sub>"] & x10["$$X$$<sub>10</sub>"]

    x11 L_x11_x111_0@--> x111["$$X$$<sub>111</sub>"] & x110["$$X$$<sub>110</sub>"]

    x10 L_x10_x101_0@--> x101["$$X$$<sub>101</sub>"] & x100["$$X$$<sub>100</sub>"]

    x111 --- Ex111["$$R\phi_1p_2\lambda$$"]

    x110 --- Ex110["$$R\phi_1p_2(1-\lambda)$$"]

    x101 --- Ex101["$$R\phi_1(1-p_2)\lambda$$"]

    x100 --- Ex100["$$R[1-\phi_1p_2-\phi_1(1-p_2)\lambda]$$"]

  

    a@{ shape: text}

    b@{ shape: text}

    c@{ shape: text}

    d@{ shape: text}

    Ex111@{ shape: text}

    Ex110@{ shape: text}

    Ex101@{ shape: text}

    Ex100@{ shape: text}

    linkStyle 0 stroke:#FFFFFF,fill:none

    linkStyle 1 stroke:#FFFFFF

    linkStyle 2 stroke:#FFFFFF,fill:none

    linkStyle 6 stroke:#000000,fill:none

    linkStyle 7 stroke:#000000,fill:none

    linkStyle 8 stroke:#000000,fill:none

    linkStyle 9 stroke:none,fill:none

    linkStyle 10 stroke:#FFFFFF,fill:none

    linkStyle 11 stroke:#FFFFFF,fill:none

    linkStyle 12 stroke:#FFFFFF,fill:none

  

    L_R_x11_0@{ curve: linear }

    L_R_x10_0@{ curve: linear }

    L_x11_x111_0@{ curve: linear }

    L_x10_x101_0@{ curve: linear }

    L_x10_x100_0@{ curve: linear }
```


[mermaid live editor](https://mermaid.live/edit#pako:eNpVjc1ugzAQhF_F2lMrkciQHxIfKjWkzSVSe8ipkMMqLBgl2MgYpSnw7jVEVds9rHY038y2cNIpgYDsoq8nicaywzZRzM1zHElT1LbE-sgmk6duR5aVWtGtY5uHnWa11FVVqPzxzm8GiEXtfsCIWVmoc3-3ojH_pqhj23iPldXV8a9zuOqOvcTFu3T1_x1pyKVe4wxFhpMTGhahGRHwIDdFCsKahjwoyZQ4SGgHNwErqaQEhDtTNOcEEtW7TIXqQ-vyJ2Z0k0tw3ZfaqaZK0dK2wNzgL0IqJRPpRlkQfjhWgGjhE8TMX0154M-CMFy7vVx7cAOxCKd8GcxDvuRzzn2-7j34Gn_y6Spc9N9AEHK1)