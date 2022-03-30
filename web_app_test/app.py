from flask import Flask, request, render_template
import pandas as pd
import joblib

# Declare a Flask app
app = Flask(__name__)

# Main function here
# ------------------
@app.route('/', methods=['GET', 'POST'])
def main():
    
    # If a form is submitted
    if request.method == "POST":
        
        #Mortality Prediction
        model= joblib.load("TAKI_Mortality_Fullmodel.pkl")
        
        mort_feature_names = ['UrineOutput_D0toD3', 'Vasopressor_ICUD0toD3', 'FI02_D1_HIGH',
        'Platelets_D1_LOW', 'AGE', 'BUN_D0toD3_HIGH', 'HR_D1_HIGH',
        'LAST_KDIGO_ICU_D0toD3', 'PH_D1_LOW', 'Bilirubin_D1_HIGH',
        'MAX_KDIGO_ICU_D0toD3', 'ECMO_ICUD0toD3', 'Hours_inICUD0toD3',
        'Temperature_D1_LOW', 'Temperature_D1_HIGH']

        # MAKE_features_names = ['LAST_KDIGO_ICU_D0toD3', 'UrineOutput_D0toD3', 'MAX_KDIGO_ICU_D0toD3',
        # 'Bilirubin_D1_HIGH', 'AGE', 'BUN_D0toD3_HIGH', 'Hemoglobin_D1_LOW',
        # 'Platelets_D1_LOW', 'FI02_D1_HIGH', 'Vasopressor_ICUD0toD3',
        # 'HR_D1_HIGH', 'PH_D1_LOW', 'Admit_sCr', 'Sodium_D1_LOW']

        mort_feature_values = []
        for f in mort_feature_names:
            cur_val =  float(request.form.get(f))
            mort_feature_values.append(cur_val)
        
        # Put inputs to dataframe
        X = pd.DataFrame([mort_feature_values], columns = mort_feature_names)
        
        # Get prediction
        prediction_class = model.predict(X)[0]  #Prediction class
        prediction_prob  = round(model.predict_proba(X)[0][1],2)  #Prediction probability
        out = "Predicted Risk is: " + str(prediction_prob)
    else:
        out = ""
        
    return render_template("index.html", output = out)

# Running the app
if __name__ == '__main__':
    #app.run(debug=True)
    app.run(host="localhost", port=5000, debug=True)