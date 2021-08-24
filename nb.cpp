/*
 NAIVE BAYES FROM SCRATCH
 
 By: Jeremiah Joseph
 */

#include <iostream>
#include <vector>
#include <fstream>
#include <math.h>
#include <chrono>

using namespace std;

//Function headers
vector<double> getApriori (vector<double> survived);
vector<double> getMean (vector<double> survived, vector<double> age, vector<double> countSurvived);
vector<double> getvariance (vector<double> survived, vector<double> age, vector<double> countSurvived, vector<double> mean);
double calcAgeLh (double v, double meanV, double varV);
vector<double> calcRawProb (double pclass, double sex, double age, double lhPclass[2][3], double lhSex [2][2], vector<double> apriori, vector<double> mean, vector<double> var);




int main(int argc, const char * argv[]) {
    // Input file stream
    ifstream inFS;
    string line;
    string x_in, pclass_in, survived_in, sex_in, age_in;
    const int MAX_LEN = 1500;
    
    // vectors for data.
    vector<double> x(MAX_LEN);
    vector<double> pclass(MAX_LEN);
    vector<double> survived(MAX_LEN);
    vector<double> sex(MAX_LEN);
    vector<double> age(MAX_LEN);


    
    // Try to open file
    cout << "Opening file titanic_project.csv." << endl;
    
    inFS.open("titanic_project.csv");
    if (!inFS.is_open()) {
       cout << "Could not open file titanic_project.csv" << endl;
       return 1; // 1 indicates error
    }
    
    cout << "Reading line 1" << endl;
    getline(inFS, line);
    
    // echo heading
    cout << "heading: " << line << endl;
    
    //getting data from file
    int numObservations = 0;
    while (inFS.good()) {
        
        //reading data
        getline(inFS, x_in, ',');
        getline(inFS, pclass_in, ',');
        getline(inFS, survived_in, ',');
        getline(inFS, sex_in, ',');
        getline(inFS, age_in, '\n');
         
        //storing data into vectors
       // x.at(numObservations) = stof(x_in);
        pclass.at(numObservations) = stof(pclass_in);
        survived.at(numObservations) = stof(survived_in);
        sex.at(numObservations) = stof(sex_in);
        age.at(numObservations) = stof(age_in);
        
        
        numObservations++;
    }
    cout << "Closing file titanic_project.csv." << endl;
    inFS.close(); // Done with file, so close it
    
    cout << "Number of records: " << numObservations << endl << endl;
    
    // divide into train and test
    vector<double>::const_iterator first = pclass.begin();
    vector<double>::const_iterator split = pclass.begin() + 900;
    vector<double>::const_iterator last = pclass.begin() + 1046;
    vector<double> pclassTrain(first, split);
    vector<double> pclassTest(split, last);
    
    vector<double>::const_iterator first2 = survived.begin();
    vector<double>::const_iterator split2 = survived.begin() + 900;
    vector<double>::const_iterator last2 = survived.begin() + 1046;
    vector<double> survivedTrain(first2, split2);
    vector<double> survivedTest(split2, last2);
    
    vector<double>::const_iterator first3 = sex.begin();
    vector<double>::const_iterator split3 = sex.begin() + 900;
    vector<double>::const_iterator last3 = sex.begin() + 1046;
    vector<double> sexTrain(first3, split3);
    vector<double> sexTest(split3, last3);
    
    vector<double>::const_iterator first4 = age.begin();
    vector<double>::const_iterator split4 = age.begin() + 900;
    vector<double>::const_iterator last4 = age.begin() + 1046;
    vector<double> ageTrain(first4, split4);
    vector<double> ageTest(split4, last4);
    
    //MACHINE LEARNING TRAINING
    auto start = std::chrono::high_resolution_clock::now();
    
    //getting apriori values
    vector<double> apriori = getApriori(survivedTrain);
    
    //getting survived vector
    vector<double> countSurvived (2);
    countSurvived[0] = apriori[0] * 900;
    countSurvived[1] = apriori[1] * 900;

    
    //getting likelihood for pclass
    double lhPclass[2][3];
    
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 3; j++) {
            double temp = 0.0;
            //counting nrow(df[df$pclass==pc & df$survived==sv,])
            for (int k = 0; k < 900; k++) {
                if (survivedTrain[k] == i && pclassTrain[k] == j + 1) {
                    temp++;
                }
            }
            temp /= countSurvived[i];
            lhPclass[i][j] = temp;
        }
    }
    
    
    //getting likelihood for sex
    double lhSex [2][2];
    
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            double temp = 0.0;
            //counting nrow(df[df$sex==sx & df$survived==sv,])
            for (int k = 0; k < 900; k++) {
                if (survivedTrain[k] == i && sexTrain[k] == j) {
                    temp++;
                }
            }
            temp /= countSurvived[i];
            lhSex[i][j] = temp;
        }
    }
    
    
    //getting likelihood for age (continuous variable).
    //getting mean vector
    vector<double> mean = getMean(survivedTrain, ageTrain, countSurvived);
    
    //getting variance vector
    vector<double> variance = getvariance(survivedTrain, ageTrain, countSurvived, mean);
    
    auto stop = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed_sec = stop-start;
    cout << "Time:" << elapsed_sec.count() << endl << endl;
    
    //printing apriori results
    cout << "Apriori: " << endl << apriori[0] << " " << apriori[1] << endl << endl;
    
    //printing liklihood pclass
    cout << "likelihood p(survived|pclass): " << endl;
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j <3; j++) {
            cout << lhPclass[i][j] << " ";
        }
        cout << endl;
    }
    cout << endl;
    
    //printing likelihood sex
    cout << "likelihood p(survived|sex): " << endl;
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j <2; j++) {
            cout << lhSex[i][j] << " ";
        }
        cout << endl;
    }
    cout << endl;
    
    //printing mean
    cout << "Age Mean: " << endl << mean[0] << " " << mean[1] << endl;

    //printing variance
    cout << "Age Variance: " << endl << variance[0] << " " << variance[1] << endl << endl;

    
    
    
    //TESTING DATA
    
    //raw probability matrix
    double rawProbsMatrix [146][2];
    
    //putting raw probabilities into the matrix
    for (int i = 0; i < 146; i++) {
        vector<double> rawProb = calcRawProb(pclassTest[i], sexTest[i], ageTest[i], lhPclass, lhSex, apriori, mean, variance);
        //putting values into probability matrix
        rawProbsMatrix[i][0] = rawProb[0];
        rawProbsMatrix[i][1] = rawProb[1];
        
    }
    
    //vector for predictions
    vector<double> predictions(146);
    
    //getting predictions
    for (int i = 0; i < 146; i++) {
        if (rawProbsMatrix[i][1] > rawProbsMatrix[i][0]) {
            predictions[i] = 1;
        }
        else {
            predictions[i] = 0;
        }
    }
    
    //calculating tp, fp, tn, fn
    double tp = 0.0, fp = 0.0, tn = 0.0, fn = 0.0;
    for(int i = 0; i < predictions.size(); i++) {
        if (predictions[i] == 1 && survivedTest[i] == 1) {
            tn++;
        }
        else if (predictions[i] == 1 && survivedTest[i] == 0) {
            fn++;
        }
        else if (predictions[i] == 0 && survivedTest[i] == 0) {
            tp++;
        }
        else if (predictions[i] == 0 && survivedTest[i] == 1) {
            fp++;
        }
    }
    //calculating and printing accuracy, specficity, and sensitivity
    double accuracy = (tp + tn) / (tp +tn +fp+fn);
    double sensitivity = (tp) / (tp + fn);
    double specificity = (tn) / (tn + fp);
    cout << "Accuracy: " << accuracy << endl;
    cout << "Sensitivity: " << sensitivity << endl;
    cout << "Specificity: " << specificity << endl;
    
    
    
    
    
    
}

//function gets apriori vector values
vector<double> getApriori (vector<double> survived) {
    double numSurvived = 0.0; double numDead = 0.0;
    vector<double> apriori (2);
    
    //seeing number that survived and did not survive.
    for (int i= 0; i< survived.size(); i++) {
        if (survived[i] == 0) {
            numDead++;
        }
        if (survived[i] == 1) {
            numSurvived++;
        }
        
    }
    //divinding by training size
    apriori[0] = numDead / survived.size();
    apriori[1] = numSurvived / survived.size();
    
    return apriori;
}

//function returns to mean vector
vector<double> getMean (vector<double> survived, vector<double> age, vector<double> countSurvived) {
    
    vector<double> mean (2);
    double ageSurvived = 0.0, ageDead = 0.0;
    //Adding respective ages
    for (int i = 0; i < survived.size(); i++) {
        if (survived[i] == 0) {
            ageDead += age[i];
        }
        else if (survived[i] == 1) {
            ageSurvived += age[i];
        }
    }
    //Dividing respective ages by count
    ageDead /= countSurvived[0];
    ageSurvived /= countSurvived[1];
    
    mean[0] = ageDead;
    mean[1] = ageSurvived;
    
    return mean;
    
}

//function returns to variance vector
vector<double> getvariance (vector<double> survived, vector<double> age, vector<double> countSurvived, vector<double> mean) {
    
    vector<double> variance (2);
    double sumSurvived = 0.0, sumDead = 0.0;
    
    //Adding respective sums
    for (int i = 0; i < survived.size(); i++) {
        if (survived[i] == 0) {
            double temp = age[i] - mean[0];
            temp = temp * temp;
            sumDead += temp;
        }
        else if (survived[i] == 1) {
            double temp = age[i] - mean[1];
            temp = temp * temp;
            sumSurvived += temp;
        }
    }
    //Dividing by respective survived category
    sumDead /= countSurvived[0] - 1 ;
    sumSurvived /= countSurvived[1] - 1 ;
    
    variance[0] = sumDead;
    variance[1] = sumSurvived;
    
    return variance;
}

// returns the likelyhood for a specific age.
double calcAgeLh (double v, double meanV, double varV) {
    double likelihood = 1 / sqrt(2 * M_PI * varV) * exp(-1 *(pow((v-meanV),2))/(2 * varV));
    return likelihood;
}

//Returns vector of probability survived and die
vector<double> calcRawProb (double pclass, double sex, double age, double lhPclass[2][3], double lhSex [2][2], vector<double> apriori, vector<double> mean, vector<double> var) {
    //rawProb vector
    vector <double> rawProb (2);
    
    //Calculating parts to Baye's formula
    double numS = lhPclass[1][int(pclass - 1)] * lhSex[1][int(sex)] * apriori[1] * calcAgeLh(age, mean[1], var[1]);
    double numP = lhPclass[0][ int(pclass - 1)] * lhSex[0][int(sex)] * apriori[0] * calcAgeLh(age, mean[0], var[0]);
    double denominator = lhPclass[1][int(pclass - 1)] * lhSex[1][int(sex)] * calcAgeLh(age, mean[1], var[1]) * apriori[1] + lhPclass[0][int(pclass - 1)] *lhSex[0][int(sex)] * calcAgeLh(age, mean[0], var[0]) * apriori[0];
    
    //Getting raw probabilities
    rawProb[0] = numP / denominator;
    rawProb[1] = numS / denominator;
    
    return rawProb;
}
