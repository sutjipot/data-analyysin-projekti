# data-analyysin-projekti
Data analysis project on how income level and the metropolitan area are related to voting behavior in Finland.

There are two pre-prepared datasets:
income2017.csv : contains information about Finland the number of taxable income recipients, annual income and these of taxes paid by municipality in 2017. The variables of the data
are:
• Tulonsaajia: The number of taxable income recipients in the area
• Tulot: The average taxable income of the region’s income recipients in euros
• Mediaanitulot: The taxable median income of the income recipients in the region in euros
• Ansiotulot: The average earned income of the area’s income recipients in euros
• Pääomatulot: The average investement (capital) income of the area’s income recipients in
euros
• Valtionvero: The average state tax paid by the income recipients of the region in euros
• Kunnallisvero: The average municipal tax paid by income recipients of the area iin euros
• Verot: Total average taxes for income recipients in the area.
• Tulot_miinus_verot: The average income after the taxes of the income recipients in the area.

The number of municipalities differs slightly from the number of municipalities in the parliamentary
election statistics ek2019.csv ( e.g. Åland (Ahvenanmaa) municipalities are missing from the election
statistics), but the data are can be connected by the name of the municipality.

In this project, we explore based on these two data sets (tulot2017.csv and ek2019.csv), how the
income level and the metropolitan area ( e.g. the voting area) are related to voting behavior in
Finland.
