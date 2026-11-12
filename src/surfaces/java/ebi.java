// This file has been automatically generated. Manual changes will be overridden.

class ebi {
    static {
        try {
            System.loadLibrary("ebi");
            System.out.println("Ebi library loaded");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static native String callEbi(String command_name, String output_format, String[] inputs);

	static native String analyseAllTraces(String arg0);
	static native String analyseCohortAnalysis(String arg0, String arg1, String arg2);
	static native String analyseCompleteness(String arg0);
	static native String analyseCoverage(String arg0, String arg1);
	static native String analyseDirectlyFollowsEdgeDifference(String arg0, String arg1);
	static native String analyseDirectlyFollowsEdgeDifferenceNoFrequencies(String arg0, String arg1);
	static native String analyseEntropy(String arg0);
	static native String analyseMedoid(String arg0, String arg1);
	static native String analyseMinimumProbabilityTraces(String arg0, String arg1);
	static native String analyseMode(String arg0);
	static native String analyseMostLikelyTraces(String arg0, String arg1);
	static native String analyseVariety(String arg0);
	static native String analyseNonStochasticActivities(String arg0);
	static native String analyseNonStochasticAnyTraces(String arg0);
	static native String analyseNonStochasticBounded(String arg0);
	static native String analyseNonStochasticCluster(String arg0, String arg1);
	static native String analyseNonStochasticEmptyTraces(String arg0);
	static native String analyseNonStochasticExecutions(String arg0, String arg1);
	static native String analyseNonStochasticInfinitelyManyTraces(String arg0);
	static native String analyseNonStochasticMedoid(String arg0, String arg1);
	static native String analyseNonStochasticTimestampsOrdered(String arg0);
	static native String associationAllTraceAttributes(String arg0, String arg1);
	static native String associationTraceAttribute(String arg0, String arg1, String arg2);
	static native String conformanceChiSquared(String arg0, String arg1);
	static native String conformanceChiSquaredSample(String arg0, String arg1, String arg2);
	static native String conformanceEarthMovers(String arg0, String arg1);
	static native String conformanceEarthMoversSample(String arg0, String arg1, String arg2);
	static native String conformanceEntropicRelevance(String arg0, String arg1);
	static native String conformanceGainPrecision(String arg0, String arg1, String arg2);
	static native String conformanceGainRecall(String arg0, String arg1, String arg2);
	static native String conformanceHellinger(String arg0, String arg1);
	static native String conformanceHellingerSample(String arg0, String arg1, String arg2);
	static native String conformanceJensenShannon(String arg0, String arg1);
	static native String conformanceJensenShannonSample(String arg0, String arg1, String arg2);
	static native String conformanceMarkovian(String arg0, String arg1, String arg2, String arg3);
	static native String conformanceUnitEarthMovers(String arg0, String arg1);
	static native String conformanceUnitEarthMoversSample(String arg0, String arg1, String arg2);
	static native String conformanceNonStochasticAlignments(String arg0, String arg1);
	static native String conformanceNonStochasticEscapingEdgesPrecision(String arg0, String arg1);
	static native String conformanceNonStochasticFittingTraces(String arg0);
	static native String conformanceNonStochasticSetAlignments(String arg0, String arg1);
	static native String conformanceNonStochasticTraceFitness(String arg0);
	static native String convertBusinessProcessModelAndNotation(String arg0);
	static native String convertFiniteLanguage(String arg0);
	static native String convertFiniteStochasticLanguage(String arg0);
	static native String convertLabelledPetriNet(String arg0);
	static native String convertLog(String arg0);
	static native String convertStochasticDeterministicFiniteAutomaton(String arg0);
	static native String convertStochasticDirectlyFollowsModel(String arg0);
	static native String convertStochasticLabelledPetriNet(String arg0);
	static native String convertStochasticNondeterministicFiniteAutomaton(String arg0);
	static native String discoverAlergia(String arg0, String arg1);
	static native String discoverAlignmentsStochasticBusinessProcessModelAndNotation(String arg0, String arg1);
	static native String discoverAlignmentsStochasticLabelledPetriNets(String arg0, String arg1);
	static native String discoverCanItBeDoneOptimally(String arg0, String arg1);
	static native String discoverDirectlyFollowsGraph(String arg0, String arg1);
	static native String discoverOccurrenceStochasticBusinessProcessModelAndNotation(String arg0, String arg1);
	static native String discoverOccurrenceStochasticLabelledPetriNet(String arg0, String arg1);
	static native String discoverOccurrenceStochasticProcessTree(String arg0, String arg1);
	static native String discoverRandomStochasticBusinessProcessModelAndNotation(String arg0);
	static native String discoverRandomStochasticLabelledPetriNet(String arg0);
	static native String discoverRandomStochasticProcessTree(String arg0);
	static native String discoverUniformStochasticBusinessProcessModelAndNotation(String arg0);
	static native String discoverUniformStochasticLabelledPetriNet(String arg0);
	static native String discoverUniformStochasticProcessTree(String arg0);
	static native String discoverNonStochasticFlowerDeterministicFiniteAutomaton(String arg0);
	static native String discoverNonStochasticFlowerProcessTree(String arg0);
	static native String discoverNonStochasticInductiveMiner(String arg0);
	static native String discoverNonStochasticInductiveMinerInfrequent(String arg0, String arg1);
	static native String discoverNonStochasticPrefixTreeDeterministicFiniteAutomaton(String arg0);
	static native String discoverNonStochasticPrefixTreeProcessTree(String arg0);
	static native String discoverNonStochasticSplitMiner(String arg0, String arg1, String arg2);
	static native String discoverNonStochasticTraceModel(String arg0);
	static native String filterLanguageTracesEmpty(String arg0);
	static native String filterLanguageTracesEventActivity(String arg0, String arg1, String arg2);
	static native String filterLanguageTracesLength(String arg0, String arg1, String arg2);
	static native String filterLogTracesEmpty(String arg0);
	static native String filterLogTracesEventActivity(String arg0, String arg1, String arg2);
	static native String filterLogTracesLength(String arg0, String arg1, String arg2);
	static native String filterStochasticLanguageTracesEmpty(String arg0);
	static native String filterStochasticLanguageTracesEventActivity(String arg0, String arg1, String arg2);
	static native String filterStochasticLanguageTracesLeastLikely(String arg0, String arg1);
	static native String filterStochasticLanguageTracesLength(String arg0, String arg1, String arg2);
	static native String filterStochasticLanguageTracesMostLikely(String arg0, String arg1);
	static native String information(String arg0);
	static native String probabilityLog(String arg0, String arg1);
	static native String reduceLabelledPetriNet(String arg0);
	static native String reduceProcessTree(String arg0);
	static native String sampleFolds(String arg0, String arg1, String arg2, String arg3);
	static native String samplePartiallyOrderedTraces(String arg0, String arg1);
	static native String sampleTraces(String arg0, String arg1);
	static native String testBootstrapTest(String arg0, String arg1, String arg2, String arg3);
	static native String testLogCategoricalAttribute(String arg0, String arg1, String arg2, String arg3);
	static native String testPermutationTestLogModel(String arg0, String arg1, String arg2, String arg3);
	static native String visualiseGraph(String arg0);
	static native String visualiseText(String arg0);

}
