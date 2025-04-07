**Overview**</br>
</br>
This project and its associated codebase are designed to support valuation and risk analysis of financial derivatives, particularly interest rate swaps. The tools enable pricing, Value at Risk (VaR) calculations, and forward-looking risk simulations under various market scenarios.</br>
</br>
By leveraging this code—or selected modules within—it is possible not only to value other types of derivatives but also to analyze portfolios consisting of tens of thousands of financial contracts in approximately 10 minutes.</br>
</br>
**Structure**</br>
</br>
The core of the project is the simulator file, which orchestrates the execution of all underlying modules. Each module is designed with a clear separation of functionality:</br>
</br>
**1. Calendar Module**</br>
Manages business days and holidays over the next 20 years, ensuring accurate scheduling for risk and valuation processes.</br>
</br>
**2. Cashflow Module**</br>
Generates expected future cash flows for individual financial contracts over a 20-year horizon.</br>
</br>
**3. Curve Module**</br>
Constructs interest rate curves for up to 20 years, tailored for different purposes such as mark-to-market (MtM) valuation, initial margin (IM) estimation, and stressed scenarios (ST).</br>
</br>
**4. PV Modules**</br>
Calculate the final valuation amounts and risk metrics across multiple scenarios, based on the inputs and curves generated in previous steps.</br>
</br>
**Applications**</br>
</br>
The modular structure makes it easy to customize and extend for various financial instruments or portfolio types. Whether you're performing real-time valuation, stress testing, or large-scale portfolio risk analysis, this framework offers a flexible and high-performance solution.

