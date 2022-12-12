# Text-Analytics-using-R
Text analytics of wireless headphones reviews on Amazon.com
Recent mass adoption of mobile phone applications has created a huge opportunity for the wireless headphone accessories. The evolution in the entertainment and fitness industry has fueled increasing demand and access to music and video access on the go. Also, the rollout of innovative features such as Active Noise Cancellation (ANC), health sensors, touch controls and applications have created tremendous value for customers.
Bose, Apple, Sony, Samsung Sennheiser, Jabra, Xiaomi and Skullcandy have the major brands in this competitive market. This report focuses on products from Bose, Sony and Sennheiser in deriving business insights. 
Methodology:
1. Data scraping
Product reviews were analyzed for different classes of products with the aim of acquiring adequate unstructured data related to the headphones industry. A total of 6,472 observations and 7 variables were acquired. The variables are location information and are as important as the ‘reviews’ text targeted for analysis. Some of the variables are star ratings, product, verified purchase and text.

2. Data cleaning and massage
The acquired data needed to be put in a ready format for subsequent analysis. This was done by renaming some observations such as the product name to the brand name (e.g. Bose) and the ‘reviews’ variable to ‘text’. Since we scraped each product individually, there was a need to bind the three data frames into one. This was done using the rbind function in R to achieve a huge dataset ready for analysis.

3. Text analysis
Adopting different text mining frameworks, the text was tokenized, grouped, counted and stop-words (common words) were removed before subsequent analysis. This is part of descriptive statistics upon which we can build other frameworks for predictive abilities.

Conclusion
Application of sentiment analysis to the text (customer reviews) gave a total of negative 22,288 sentiments using the AFINN lexicon. The AFINN lexicon rates sentiments with numerical values between negative five (-5) and positive five (+5). This shows that there’s a lot of dissatisfaction amongst customers and an opportunity for current and prospective companies to take advantage of.
A review of the words with significant business value indicates that business should place more attention to design (sexiness), price, sound quality and the presence of an ecosystem which will render after sales support


