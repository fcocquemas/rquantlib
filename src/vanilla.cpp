
//  RQuantLib -- R interface to the QuantLib libraries
//
//  Copyright (C) 2002 - 2019  Dirk Eddelbuettel <edd@debian.org>
//
//  This file is part of RQuantLib.
//
//  RQuantLib is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 2 of the License, or
//  (at your option) any later version.
//
//  RQuantLib is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with RQuantLib.  If not, see <http://www.gnu.org/licenses/>.

#include <rquantlib_internal.h>

// [[Rcpp::interfaces(r, cpp)]]

// [[Rcpp::export]]
Rcpp::List vanillaOptionEngine(std::string exerciseType,
                               std::string type,
                               double underlying,
                               double strike,
                               double dividendYield,
                               double riskFreeRate,
                               double maturity,
                               double volatility,
                               Rcpp::Nullable<Rcpp::NumericVector> discreteDividends,
                               Rcpp::Nullable<Rcpp::NumericVector> discreteDividendsTimeUntil,
                               std::string engine,
                               int timeSteps,
                               int gridPoints) {

#ifdef QL_HIGH_RESOLUTION_DATE
    // in minutes
    boost::posix_time::time_duration length = boost::posix_time::minutes(boost::uint64_t(maturity * 360 * 24 * 60));
#else
    int length           = int(maturity*360 + 0.5); // FIXME: this could be better
#endif
    
    QuantLib::Option::Type optionType = getOptionType(type);
    // QuantLib::Date today = QuantLib::Date::todaysDate();
    QuantLib::Date today(24, QuantLib::Dec, 2018);
    
    QuantLib::Settings::instance().evaluationDate() = today;
    
#ifdef QL_HIGH_RESOLUTION_DATE
    QuantLib::Date exDate(today.dateTime() + length);
#else
    QuantLib::Date exDate = today + length;
#endif
    
    // new framework as per QuantLib 0.3.5
    QuantLib::DayCounter dc = QuantLib::Actual360();
    QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> spot(new QuantLib::SimpleQuote( underlying ));
    QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> vol(new QuantLib::SimpleQuote( volatility ));
    QuantLib::ext::shared_ptr<QuantLib::BlackVolTermStructure> volTS = flatVol(today, vol, dc);
    QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> qRate(new QuantLib::SimpleQuote( dividendYield ));
    QuantLib::ext::shared_ptr<QuantLib::YieldTermStructure> qTS = flatRate(today, qRate, dc);
    QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> rRate(new QuantLib::SimpleQuote( riskFreeRate ));
    QuantLib::ext::shared_ptr<QuantLib::YieldTermStructure> rTS = flatRate(today, rRate, dc);
    
    bool withDividends = discreteDividends.isNotNull() && discreteDividendsTimeUntil.isNotNull();
    
    QuantLib::ext::shared_ptr<QuantLib::StrikedTypePayoff> payoff(new QuantLib::PlainVanillaPayoff(optionType, strike));
    
    if (withDividends) { // option is DividendVanillaOption
        Rcpp::NumericVector divvalues(discreteDividends), divtimes(discreteDividendsTimeUntil);
        int n = divvalues.size();
        std::vector<QuantLib::Date> discDivDates(n);
        std::vector<double> discDividends(n);
        for (int i = 0; i < n; i++) {
#ifdef QL_HIGH_RESOLUTION_DATE
            boost::posix_time::time_duration discreteDividendLength = boost::posix_time::minutes(boost::uint64_t(divtimes[i] * 360 * 24 * 60));
            discDivDates[i] = QuantLib::Date(today.dateTime() + discreteDividendLength);
#else
            discDivDates[i] = today + int(divtimes[i] * 360 + 0.5);
#endif
            discDividends[i] = divvalues[i];
        }
        
        QuantLib::ext::shared_ptr<QuantLib::PricingEngine> pricingEngine;
        QuantLib::ext::shared_ptr<QuantLib::DividendVanillaOption> option;
        
        if(exerciseType == "American") {
            QuantLib::ext::shared_ptr<QuantLib::Exercise> exercise(new QuantLib::AmericanExercise(today, exDate));
            pricingEngine = makeDividendPricingEngine(spot, qTS, rTS, volTS, engine, timeSteps, gridPoints);
            option.reset(new QuantLib::DividendVanillaOption(payoff, exercise, discDivDates, discDividends));
            // option = std::make_shared<QuantLib::DividendVanillaOption>(payoff, exercise, discDivDates, discDividends);
        } else if(exerciseType == "European") {
            QuantLib::ext::shared_ptr<QuantLib::Exercise> exercise(new QuantLib::EuropeanExercise(exDate));
            pricingEngine = makeDividendPricingEngine(spot, qTS, rTS, volTS, engine, timeSteps, gridPoints);
            option.reset(new QuantLib::DividendVanillaOption(payoff, exercise, discDivDates, discDividends));
            // option = std::make_shared<QuantLib::DividendVanillaOption>(payoff, exercise, discDivDates, discDividends);
        } else {
            QL_FAIL("Unknown exercise type");
        }
        
        option->setPricingEngine(pricingEngine);
        
        const QuantLib::PricingEngine::results* results;
        option->fetchResults(results);
        Rcpp::Rcout << "Results: " << results << std::endl;
        
        return Rcpp::List::create(Rcpp::Named("value") = option->NPV(),
                                  Rcpp::Named("delta") = R_NaReal, // option.delta(),
                                  Rcpp::Named("gamma") = R_NaReal, // option.gamma(),
                                  Rcpp::Named("vega") = R_NaReal, // option.vega(),
                                  Rcpp::Named("theta") = R_NaReal, // option.theta(),
                                  Rcpp::Named("rho") = R_NaReal, // option.rho(),
                                  Rcpp::Named("divRho") = R_NaReal);
    }
    else { // option is VanillaOption
        QuantLib::ext::shared_ptr<QuantLib::PricingEngine> pricingEngine;
        QuantLib::ext::shared_ptr<QuantLib::VanillaOption> option;
        
        if(exerciseType == "American") {
            QuantLib::ext::shared_ptr<QuantLib::Exercise> exercise(new QuantLib::AmericanExercise(today, exDate));
            pricingEngine = makePricingEngine(spot, qTS, rTS, volTS, 
                                              engine, timeSteps, gridPoints);
            option.reset(new QuantLib::VanillaOption(payoff, exercise));
        } else if(exerciseType == "European") {
            QuantLib::ext::shared_ptr<QuantLib::Exercise> exercise(new QuantLib::EuropeanExercise(exDate));
            pricingEngine = makePricingEngine(spot, qTS, rTS, volTS, 
                                              engine, timeSteps, gridPoints);
            option.reset(new QuantLib::EuropeanOption(payoff, exercise));
        } else {
            QL_FAIL("Unknown exercise type");
        }

        option->setPricingEngine(pricingEngine);
        
        // const QuantLib::PricingEngine::results* results;
        // option->fetchResults(results);
        // const QuantLib::Instrument::results* results = 
        //     dynamic_cast<const QuantLib::Instrument::results*>(pricingEngine->getResults());
        // Rcpp::Rcout << "Results: " << results->delta << std::endl;
        
        return Rcpp::List::create(Rcpp::Named("value") = option->NPV(),
                                  Rcpp::Named("delta") = R_NaReal, // option->delta(),
                                  Rcpp::Named("gamma") = R_NaReal, // option->gamma(),
                                  Rcpp::Named("vega") = R_NaReal, // option->vega(),
                                  Rcpp::Named("theta") = R_NaReal, // option->theta(),
                                  Rcpp::Named("rho") = R_NaReal, // option->rho(),
                                  Rcpp::Named("divRho") = R_NaReal); // option->dividendRho());
    }
    
}


// [[Rcpp::export]]
Rcpp::List europeanOptionArraysEngine(std::string type, Rcpp::NumericMatrix par) {

    QuantLib::Option::Type optionType = getOptionType(type);
    int n = par.nrow();
    Rcpp::NumericVector value(n), delta(n), gamma(n), vega(n), theta(n), rho(n), divrho(n);

    QuantLib::Date today = QuantLib::Date::todaysDate();
    QuantLib::Settings::instance().evaluationDate() = today;

    QuantLib::DayCounter dc = QuantLib::Actual360();

    for (int i=0; i<n; i++) {

        double underlying    = par(i, 0);    // first column
        double strike        = par(i, 1);    // second column
        QuantLib::Spread dividendYield = par(i, 2);    // third column
        QuantLib::Rate riskFreeRate    = par(i, 3);    // fourth column
        QuantLib::Time maturity        = par(i, 4);    // fifth column
#ifdef QL_HIGH_RESOLUTION_DATE
        // in minutes
        boost::posix_time::time_duration length = boost::posix_time::minutes(boost::uint64_t(maturity * 360 * 24 * 60));
#else
        int length           = int(maturity*360 + 0.5); // FIXME: this could be better
#endif
        double volatility    = par(i, 5);    // sixth column

        QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> spot(new QuantLib::SimpleQuote( underlying ));
        QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> vol(new QuantLib::SimpleQuote( volatility ));
        QuantLib::ext::shared_ptr<QuantLib::BlackVolTermStructure> volTS = flatVol(today, vol, dc);
        QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> qRate(new QuantLib::SimpleQuote( dividendYield ));
        QuantLib::ext::shared_ptr<QuantLib::YieldTermStructure> qTS = flatRate(today, qRate, dc);
        QuantLib::ext::shared_ptr<QuantLib::SimpleQuote> rRate(new QuantLib::SimpleQuote( riskFreeRate ));
        QuantLib::ext::shared_ptr<QuantLib::YieldTermStructure> rTS = flatRate(today, rRate, dc);

#ifdef QL_HIGH_RESOLUTION_DATE
    QuantLib::Date exDate(today.dateTime() + length);
#else
    QuantLib::Date exDate = today + length;
#endif
        QuantLib::ext::shared_ptr<QuantLib::Exercise> exercise(new QuantLib::EuropeanExercise(exDate));

        QuantLib::ext::shared_ptr<QuantLib::StrikedTypePayoff> payoff(new QuantLib::PlainVanillaPayoff(optionType, strike));
        QuantLib::ext::shared_ptr<QuantLib::VanillaOption> option = makeOption(payoff, exercise, spot, qTS, rTS, volTS, Analytic);

        value[i]  = option->NPV();
        delta[i]  = option->delta();
        gamma[i]  = option->gamma();
        vega[i]   = option->vega();
        theta[i]  = option->theta();
        rho[i]    = option->rho();
        divrho[i] = option->dividendRho();
    }
    return Rcpp::List::create(Rcpp::Named("value")  = value,
                              Rcpp::Named("delta")  = delta,
                              Rcpp::Named("gamma")  = gamma,
                              Rcpp::Named("vega")   = vega,
                              Rcpp::Named("theta")  = theta,
                              Rcpp::Named("rho")    = rho,
                              Rcpp::Named("divRho") = divrho);
}
