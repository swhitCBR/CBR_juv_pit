

$$
CH=\{{100,101,110,111}\}
$$
Don't have to go into all of those
$$
CH=\{{S_{0}(1-p_{1})...,...,..,...}\}
$$
$$
\{{y_{100},y_{101},y_{110},y_{111}}\}
$$
$$
y=\{{0,10,5,20,1}\}
$$
$$
\hat{\pi}=\{{0.05,0.5,0.25,0.1,0.1}\}
$$
- look at how the frequency of 0s would crop up in any cell given a specific R value

see [[mermaid_diagrams]]

- What is the probability of having at least one empty cell in the two sampling occasion case $$
CH=\{{10,01,11}\}
$$
$$
P(y_{j}=0|\pi_{j},R)
$$
$$
{R \choose  n_{10},n_{01},n_{11}} \pi_{10}^{n_{10}} + \pi_{01}^{n_{01}} +\pi_{11}^{n_{11}}

$$




Multinomial probability distribution for Release size R and cell probabilities and cell counts
$$
\frac{R!}{n_{10}!,n_{01}!,n_{11}!}\pi_{10}^{n_{10}} + \pi_{01}^{n_{01}} +\pi_{11}^{n_{11}}

$$
p, S, and lambda


$\pi_{11}=S_{1} \cdot p_{1} \cdot \lambda$
$\pi_{10}=S_{1} \cdot p_{1} \cdot (1-\lambda)$
$\pi_{01}=S_{1} \cdot (1-p_{1}) \cdot \lambda$







$$
{R \choose  n_{j}} \pi_j^{n_j}+(1-\pi_j)^{R-n_j}

$$


![[Pasted image 20251223153033.png]]
if n_j is zero then
$$
={R \choose  0} \pi^{0}+(1-\pi)^{R-0}
$$
$$
=1+(1-\pi_j)^{R}
$$



$$
\frac{R!},n_{10}!n_{01}!n_{11}!}/5

$$

\pi_j^{n_j}+(1-\pi_j)^{R-n_j}
if n_j is zero then



I'm not confident in this
$$
\sum_{j=1}^{J} {1-(1-\pi_{j})^R} \\
$$


![[Pasted image 20251219175239.png]]![[Pasted image 20251219175223.png]]![[Pasted image 20251219175154.png]]![[Pasted image 20251219175122.png]]![[Pasted image 20251219174454.png]]