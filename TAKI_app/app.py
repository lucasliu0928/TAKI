from flask import Flask, request, render_template
import pandas as pd
import joblib
from sklearn.preprocessing import MinMaxScaler

# Declare a Flask app
app = Flask(__name__)


def get_feature_values(feature_names):
    feature_values = []
    for f in feature_names:
        cur_val =  request.form.get(f)
        feature_values.append(cur_val)
    return feature_values
# Main function here
# ------------------
@app.route('/', methods=['GET', 'POST'])
def main():
    # If a form is submitted
    if request.method == "POST":

        #Mortality features
        Mort_feature_names = ['UrineOutput_D0toD3', 'Vasopressor_ICUD0toD3', 'FI02_D1_HIGH',
        'Platelets_D1_LOW', 'AGE', 'BUN_D0toD3_HIGH', 'HR_D1_HIGH',
        'LAST_KDIGO_ICU_D0toD3', 'PH_D1_LOW', 'Bilirubin_D1_HIGH',
        'MAX_KDIGO_ICU_D0toD3', 'ECMO_ICUD0toD3', 'Hours_inICUD0toD3',
        'Temperature_D1_LOW', 'Temperature_D1_HIGH']
        
        #MAKE features
        MAKE_features_names = ['LAST_KDIGO_ICU_D0toD3', 'UrineOutput_D0toD3', 'MAX_KDIGO_ICU_D0toD3',
        'Bilirubin_D1_HIGH', 'AGE', 'BUN_D0toD3_HIGH', 'Hemoglobin_D1_LOW',
        'Platelets_D1_LOW', 'FI02_D1_HIGH', 'Vasopressor_ICUD0toD3',
        'HR_D1_HIGH', 'PH_D1_LOW', 'Admit_sCr', 'Sodium_D1_LOW']

        #Select model
        if request.form.get("Temperature_D1_HIGH") is not None: #if this var is not none, then this is a mortality model
            model = joblib.load("TAKI_Mortality_Fullmodel.pkl")
            scaler =  joblib.load("TAKI_Mortality_Scaler.pkl")
            feature_names = Mort_feature_names
            model_name = "Mortality"

        else:
            model = joblib.load("TAKI_MAKE_Fullmodel.pkl")
            scaler =  joblib.load("TAKI_MAKE_Scaler.pkl")
            feature_names = MAKE_features_names
            model_name = "MAKE"

        #get feature name and feature values
        feature_values = get_feature_values(feature_names)
        
        # Put inputs to dataframe
        X = pd.DataFrame([feature_values], columns = feature_names)
        X_scaled = pd.DataFrame(scaler.transform(X))
        X_scaled.columns = X.columns
        X_scaled.index = X.index

        
        # Get prediction
        prediction_class = model.predict(X)[0]  #Prediction class
        prediction_prob  = round(model.predict_proba(X)[0][1],2)  #Prediction probability


        # Output
        #Create dictionary to store input data and show output

        out = "The Predicted " + model_name + " Risk is: " + str(round(prediction_prob*100,2)) + "%"
    else:
        out = ""
        
    return render_template("index.html", output = out)

# Running the app
if __name__ == '__main__':
    app.run(debug=True)
    #app.run(host="localhost", port=5000, debug=True)