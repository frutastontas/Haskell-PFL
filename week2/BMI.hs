classifyBMI :: Float-> Float-> String
classifyBMI w h | bmi < 18.5 = "underweight"
                | bmi < 25 && bmi >= 18.5 = "normal weight"
                | bmi < 30 && bmi >= 25 = "overweight"
                | bmi >= 30 = "obese"
                where bmi = w / (h*h)
                
