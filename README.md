# Overview
The "Capital building calculator" Shiny app provides a tool for users to visualize the growth of their capital over time based on various input parameters. The app takes into account factors such as initial capital, savings rate, interest rate, and more to generate detailed plots and tables showing the development of capital over the years.

**Note:** This is a theoretical model, and the results are based on the input parameters provided by the user. Actual financial outcomes may vary based on various factors not accounted for in this model.  

Container image available here: [![Container availble on Quay](https://quay.io/repository/michard/compound_interest_calculator/status "Container availble on Quay")](https://quay.io/repository/michard/compound_interest_calculator)

App available here: [link](https://compound-calculator.michard.io)

## Input Parameters
**Start Year:** The year when the investment begins.  
**Initial Capital:** The amount of money you start with.  
**Savings Rate:** The amount of money you plan to save regularly.  
**Savings Interval:** The frequency at which you save the specified savings rate (either monthly or yearly).  
**Investment Period:** The total number of years you plan to invest.  
**Interest Rate:** The annual interest rate (as a percentage) that your capital will earn.  
**Adjustment Rate:** The annual rate (as a percentage) at which your savings rate will increase.  
**Savings Suspension:** The number of years after which you plan to stop saving money.  
**Target Value:** A specific capital value you aim to achieve. The app will indicate when (or if) this value is reached.  

## Generated Diagrams
**Overview:** Shows the growth of accumulated savings and total capital over time.  
**Distribution:** Displays a pie chart showing the distribution between total savings and total interest earned.  
**Savings Rate:** Represents the annual savings rate in relation to the value of the generated interest each year. This visualization illustrates the development of both the savings rate and the generated interest over time. Additionally, it highlights the year when the generated interest surpasses the annual savings rate.   
**Normalized Values:** Displays the values of the savings rate and generated interests, both normalized based on the annual growth comprised of the savings rate and yearly interests. This provides a clearer perspective on how each component contributes to the overall growth each year.  
**Goals:** Displays the development of total capital and highlights specific milestones, such as when the capital doubles from the initial investment. It also indicates when the user-defined target value is achieved.  
**Values:** A table that provides a detailed breakdown of the capital at the beginning of the year, savings amount per year, generated interest per year, and capital at the end of the year.  

## How to Use
1. Input your financial details and preferences in the sidebar panel.
2. Click the "Calculate" button.
3. Navigate through the tabs in the main panel to view the generated diagrams and tables based on your input.  
  
By using this app, users can gain insights into how their capital will grow over time, understand the impact of different financial decisions, and set realistic financial goals.

## Run app locally
1.  Clone this repository
```bash
git clone https://github.com/smichard/compound_interest_calculator.git
```

2. Navigate to the project directory:

```bash
cd compound_interest_calculator
```

3. Build the container image:
Run the following command to build a Docker image. Replace `my_app`` with a name of your choice for the image.
```bash
docker build -t my_app -f Containerfile
```
This command will use the provided Containerfile to build an image named `my_app``. The process will install the necessary R packages and set up the environment for the Shiny app.

4. Run the Shiny app locally:
After building the image, you can run the Shiny app locally using the following command:

```bash
docker run --rm -p 3838:3838 my_app
```
This command will start a container from the `my_app`` image and map port 3838 of the container to port 3838 of your local machine.

5. Access the Shiny app in a browser:
Open a web browser and navigate to:
```
http://localhost:3838/
```
You should now see your Shiny app running!