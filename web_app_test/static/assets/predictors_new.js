$(document).ready(function () {
    document.getElementById("mort-selection").focus({preventScroll:true});
    let selected = "make";



    $("#predict_make").click(function () {
        //
        $("#warnbox").hide();
        // $("#warntext").replaceWith("<p id='warntext'><h4>Please check the values for the following features:</h4></p>");
        $("#warntext").html("<h4><b>Warning</b>: Provided value(s) are outside the range of values in the training dataset.  If the patient's value is outside this range, the corresponding minimum/maximum value will be used. <br>Please check following features:</h4>");

        let age = checkInput("#id_age", true, "Age");
        let temp = checkInput("#id_temp", true, "Temperature");
        let hr = checkInput("#id_heartrate", true, "Heart rate");
        let map = checkInput("#id_map", true, "MAP");
        let unplanned = checkInput("#id_is_unplanned_admission", false, "Unplanned Admission");  // categorical

        // These are all categorical selections, so no checking necessary
        let ecmo = checkInput("#id_got_ecmo", false, "ECMO");
        let iabp = checkInput("#id_got_iabp", false, "IABP");
        let mechvent = checkInput("#id_got_mechvent", false, "Mechanical Ventilation");
        let sepsis = checkInput("#id_is_septic", false, "Sepsis");
        let vasopress = checkInput("#id_vasopress_exp", false, "Vasoactive Drugs");

        let bili = checkInput("#id_bili", true, "Bilirubin");
        let sodium = checkInput("#id_sodium", true, "Sodium");
        let albumin = checkInput("#id_albumin", true, "Albumin");
        let po2 = checkInput("#id_po2", true, "pO<sub>2</sub>");
        let pco2 = checkInput("#id_pco2", true, "pCO<sub>2</sub>");
        let hemoglobin = checkInput("#id_hemoglobin", true, "Hemoglobin");
        let fio2 = checkInput("#id_fio2", true, "FiO<sub>2</sub>");
        let platelets = checkInput("#id_platelets", true, "Platelets");
        let bicarb = checkInput("#id_bicarbonate", true, "Bicarbonate");
        let bun = checkInput("#id_bun", true, "BUN");
        let ph = checkInput("#id_ph", true, "pH");

        let maxkdigo = checkInput("#id_maxKDIGO_3day", true, "Maximum KDIGO Severity");
        let lastkdigo = checkInput("#id_lastKDIGO", true, "Latest KDIGO Severity");
        let rrt_flag = checkInput("#id_rrt_flag", true, "Type of RRT");
        let rrt_d3 = checkInput("#id_rrt_d3", true, "RRT Status at Day 3");
        let fluidoverload = checkInput("#id_fluidoverload", true, "Fluid Overload");
        let urine_output = checkInput("#id_urineout", true, "Hourly Urine Output");

        if (rrt_flag > 0) {
            maxkdigo = 4;
        }
        if (rrt_d3 > 0) {
            lastkdigo = 4;
        }

        let bias = 0.000811;
        let age_weight = 0.019950;
        let albumin_weight = -0.214324;
        let bicarb_weight = 0.010856;
        let bili_weight = 0.056751;
        let bun_weight = 0.002712;
        let fio2_weight = 0.008252;
        let fluidoverload_weight = 0.046096;
        let hr_weight = 0.004406;
        let hemoglobin_weight = -0.037325;
        let ecmo_weight = 0.086574;
        let iabp_weight = 0.132365;
        let mechvent_weight = 0.249266;
        let map_weight = 0.001223;
        let vasopress_weight = 0.143060;
        let pco2_weight = 0.004663;
        let ph_weight = 0.029938;
        let platelets_weight = -0.000607;
        let po2_weight = 0.001619;
        let sepsis_weight = 0.467859;
        let sodium_weight = 0.008116;
        let temp_weight = -0.141995;
        let urine_output_weight = -0.004846;
        let unplanned_weight = 0.636098;
        let maxkdigo_weight = -0.340085;
        let rrt_flag_weight = -0.115296;
        let lastkdigo_weight = 0.681653;

        let z = bias + (age * age_weight) + (temp * temp_weight) + (hr * hr_weight) + (map * map_weight) +
            (unplanned * unplanned_weight) + (ecmo * ecmo_weight) + (iabp * iabp_weight) + (mechvent * mechvent_weight) +
            (sepsis * sepsis_weight) + (vasopress * vasopress_weight) + (bili * bili_weight) + (sodium * sodium_weight) +
            (albumin * albumin_weight) + (po2 * po2_weight) + (pco2 * pco2_weight) + (hemoglobin * hemoglobin_weight) +
            (fio2 * fio2_weight) + (platelets * platelets_weight) + (bicarb * bicarb_weight) + (bun * bun_weight) +
            (ph * ph_weight) + (maxkdigo * maxkdigo_weight) + (lastkdigo * lastkdigo_weight) +
            (rrt_flag * rrt_flag_weight) + (fluidoverload * fluidoverload_weight) + (urine_output * urine_output_weight);

        let y = 1 / (1 + Math.exp(-z));

        $("#makebox").replaceWith("<div class=\"col-sm-4 text-center\" id=\"makebox\"><h3>Predicted Risk for MAKE: " +
            (y * 100).toFixed(2) + "%</h3>");

        bias = -0.008583;
        age_weight = 0.018033;
        albumin_weight = -0.205544;
        bicarb_weight = 0.006270;
        bili_weight = 0.033608;
        bun_weight = 0.003117;
        fio2_weight = -0.000382;
        fluidoverload_weight = -0.032189;
        hr_weight = 0.000230;
        hemoglobin_weight = -0.116097;
        ecmo_weight = -0.021232;
        iabp_weight = 0.028136;
        mechvent_weight = 0.090984;
        map_weight = 0.008138;
        vasopress_weight = -0.072937;
        pco2_weight = 0.000496;
        ph_weight = 0.009419;
        platelets_weight = 0.000107;
        po2_weight = 0.000577;
        sepsis_weight = 0.150139;
        sodium_weight = 0.000628;
        temp_weight = -0.082912;
        urine_output_weight = -0.001777;
        unplanned_weight = 0.286304;
        maxkdigo_weight = -0.055464;
        rrt_flag_weight = -0.145377;
        lastkdigo_weight = 0.584902;


        z = bias + (age * age_weight) + (temp * temp_weight) + (hr * hr_weight) + (map * map_weight) +
            (unplanned * unplanned_weight) + (ecmo * ecmo_weight) + (iabp * iabp_weight) + (mechvent * mechvent_weight) +
            (sepsis * sepsis_weight) + (vasopress * vasopress_weight) + (bili * bili_weight) + (sodium * sodium_weight) +
            (albumin * albumin_weight) + (po2 * po2_weight) + (pco2 * pco2_weight) + (hemoglobin * hemoglobin_weight) +
            (fio2 * fio2_weight) + (platelets * platelets_weight) + (bicarb * bicarb_weight) + (bun * bun_weight) +
            (ph * ph_weight) + (maxkdigo * maxkdigo_weight) + (lastkdigo * lastkdigo_weight) +
            (rrt_flag * rrt_flag_weight) + (fluidoverload * fluidoverload_weight) + (urine_output * urine_output_weight);

        y = 1 / (1 + Math.exp(-z));

        $("#survbox").replaceWith("<div class=\"col-sm-4 text-center\" id=\"survbox\"><h3>Predicted Risk If Survived: " +
            (y * 100).toFixed(2) + "%</h3>");
        // $("#warnbox").show();
    });

    $("#predict_mort").click(function () {

        let age = parseFloat($("#id_age").val());
        let hr = parseFloat($("#id_heartrate").val());
        let map = parseFloat($("#id_map").val());
        let unplanned = parseFloat($("#id_is_unplanned_admission").val());

        let mhs = parseFloat($("#id_got_mhs").val());
        let mechvent = parseFloat($("#id_got_mechvent").val());
        let sepsis = parseFloat($("#id_is_septic").val());
        let vasopress = parseFloat($("#id_vasopress_exp").val());

        let bili = parseFloat($("#id_bili").val());
        let sodium = parseFloat($("#id_sodium").val());
        let albumin = parseFloat($("#id_albumin").val());
        let po2 = parseFloat($("#id_po2").val());
        let pco2 = parseFloat($("#id_pco2").val());
        let fio2 = parseFloat($("#id_fio2").val());
        let platelets = parseFloat($("#id_platelets").val());
        let bicarb = parseFloat($("#id_bicarbonate").val());
        let ph = parseFloat($("#id_ph").val());

        let maxkdigo = parseFloat($("#id_maxKDIGO_3day").val());
        let lastkdigo = parseFloat($("#id_lastKDIGO").val());
        let rrt_flag = parseFloat($("#id_rrt_flag").val());
        let fluidoverload = parseFloat($("#id_fluidoverload").val());

        let bias = -0.077038;
        let age_weight = 0.019848;
        let albumin_weight = -0.028490;
        let bicarb_weight = 0.009171;
        let bili_weight = 0.069971;
        let fio2_weight = 0.011915;
        let fluidoverload_weight = 0.103502;
        let hr_weight = 0.004818;
        let mechvent_weight = 0.408476;
        let map_weight = -0.008526;
        let vasopress_weight = 0.336014;
        let pco2_weight = 0.005027;
        let ph_weight = -0.649560;
        let platelets_weight = -0.001439;
        let po2_weight = 0.001824;
        let sepsis_weight = 0.644460;
        let sodium_weight = -0.002430;
        let mhs_weight = 0.452904;
        let unplanned_weight = 0.896007;
        let maxkdigo_weight = -0.684032;
        let rrt_flag_weight = 0.077903;
        let lastkdigo_weight = 0.710822;


        let z = bias + (age * age_weight) + (hr * hr_weight) + (map * map_weight) +
            (unplanned * unplanned_weight) + (mhs * mhs_weight) + (mechvent * mechvent_weight) +
            (sepsis * sepsis_weight) + (vasopress * vasopress_weight) + (bili * bili_weight) + (sodium * sodium_weight) +
            (albumin * albumin_weight) + (po2 * po2_weight) + (pco2 * pco2_weight) +
            (fio2 * fio2_weight) + (platelets * platelets_weight) + (bicarb * bicarb_weight) +
            (ph * ph_weight) + (maxkdigo * maxkdigo_weight) + (lastkdigo * lastkdigo_weight) +
            (rrt_flag * rrt_flag_weight) + (fluidoverload * fluidoverload_weight);

        let y = 1 / (1 + Math.exp(-z));

        $("#mortbox").replaceWith("<div class=\"col-sm-4 text-center\" id=\"mortbox\"><h3>Predicted Risk for Hospital Mortality: " +
            (y * 100).toFixed(2) + "%</h3>");
        // });
    });


    $("#foCalcButton").click(function () {
        let weight = parseFloat($("#id_fo_weight").val());
        let cfb = parseFloat($("#id_fo_cfb").val());
        $("#id_fluidoverload").val((cfb / weight * 100).toFixed(2));
        $("#foModal").modal("hide");
    });

    $("#uopCalcButton").click(function () {
        let uout = parseFloat($("#id_uop").val());
        let hrs = parseFloat($("#id_icu_hrs").val());
        $("#id_urineout").val((uout / hrs).toFixed(2));
        $("#uoModal").modal("hide");
    });

    $("#mort-selection").click(function() {
        $("#mhs-group").show();
        $("#temp-group").hide();
        $("#iabp-group").hide();
        $("#ecmo-group").hide();
        $("#uop-group").hide();
        $("#hemoglobin-group").hide();
        $("#bun-group").hide();
        $("#mort-selection").addClass("focus");
        $("#make-selection").removeClass("focus");
        $("#result-row").html("<div class=\"col-sm-3\"></div>\n" +
            "                <div class=\"col-sm-6\" id=\"mortbox\"></div>\n" +
            "                <div class=\"col-sm-3\"></div>");
        $("#submit-div").html("<button class=\"btn btn-warning btn-lg btn-full-width\" style=\"margin-bottom: 10px; white-space: normal;\" name=\"predict_baseline_dev\", id=\"predict_mort\">\n" +
            "            <span class=\"glyphicon glyphicon-play\" style=\"margin-right: 5px\"></span> Compute prediction</button>");
    });

    $("#make-selection").click(function() {
        $("#mhs-group").hide();
        $("#temp-group").show();
        $("#iabp-group").show();
        $("#ecmo-group").show();
        $("#uop-group").show();
        $("#hemoglobin-group").show();
        $("#bun-group").show();
        $("#mort-selection").removeClass("focus");
        $("#make-selection").addClass("focus");
        $("#result-row").html("<div class=\"col-sm-2\"></div>\n" +
            "                <div class=\"col-sm-4\" id=\"makebox\"></div>\n" +
            "                <div class=\"col-sm-4\" id=\"survbox\"></div>\n" +
            "                <div class=\"col-sm-2\"></div>");

        $("#submit-div").html("<button class=\"btn btn-warning btn-lg btn-full-width\" style=\"margin-bottom: 10px; white-space: normal;\" name=\"predict_baseline_dev\", id=\"predict_make\">\n" +
            "            <span class=\"glyphicon glyphicon-play\" style=\"margin-right: 5px\"></span> Compute prediction</button>");
    });
});

function checkInput(inputId, checkRange, featureName) {
    let link = document.querySelector(inputId);
    let value = parseFloat($(inputId).val());
    if ($(inputId).val() ==="") {
        $(inputId).css('border-color', 'red');
        $(inputId).css('border-width', '2px');
        $(inputId).next().show();
        $("#warntext").append("<br><b>" + featureName + "</b>: Please make a selection or enter a value.");
        $("#warnbox").show();
    }
    else if (checkRange) {
        let minVal = parseFloat(link.getAttribute("min"));
        let maxVal = parseFloat(link.getAttribute("max"));
        if ((value < minVal) || (value > maxVal)) {
            $(inputId).css('border-color', 'red');
            $(inputId).css('border-width', '2px');
            $("#warntext").append("<br><b>" + featureName + "</b>: Provided value of " + value.toFixed(2) + " is outside training range of (" + minVal.toFixed(2) + " - " + maxVal.toFixed(2) +")");
            if (value < minVal) value = minVal
            else value = maxVal;
            $(inputId).next().show();
            $("#warnbox").show();

        } else {
            $(inputId).css('border-color', '');
            $(inputId).css('border-width', '');
            $(inputId).next().hide();
        }
    }
    return value;
}

