**Overview**</br>
</br>
This project explores techniques for portfolio compression, which aims to reduce the size of large-scale portfolios—typically consisting of thousands of financial contracts—without significantly altering their key risk characteristics.</br>
</br>
As portfolios grow through the accumulation of similar contracts, their notional size may increase substantially, even though the actual risk exposure does not grow proportionally. To address this, markets commonly use compression techniques to generate an alternative portfolio that maintains the original risk profile while minimizing notional and operational complexity.</br>
</br>
This repository contains code that analyzes such techniques and applies various optimization methods to achieve portfolio reduction.</br>
</br>
**Structure**</br>
</br>
**Dashboard and Test Files**</br>
Provide tools for analyzing the risk characteristics of portfolios before and after applying compression techniques.</br>
</br>
**LPcomp Files**</br>
Implement portfolio compression using linear programming, optimizing for minimal notional while preserving risk factors.</br>
</br>
**Solocomp Files**</br>
Apply offsetting and contract adjustment methods to reduce portfolio size, reflecting more practical, rule-based compression strategies.</br>
</br>
**Applications**</br>
</br>
These tools support the design and evaluation of portfolio compression strategies, enabling institutions to streamline operations and reduce exposure while maintaining risk integrity. The modular design allows for customization and testing across different types of portfolios and compression objectives.
