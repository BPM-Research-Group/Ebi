// This file has been automatically generated. Manual changes will be overridden.

package org.ebitools;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Date;

public class Ebi {
    static {
		try {
			// load from path initially
			System.loadLibrary("ebi");
			System.out.println("Ebi library loaded from path.");
        } catch (UnsatisfiedLinkError e) {
			// if that's not available, load from JAR
			try {
				String path = "Ebi_" + new Date().getTime();
				String os = System.getProperty("os.name").toLowerCase();
				InputStream in;
				File fileOut;
				if (os.contains("win")) {
					// windows
					in = Ebi.class.getResourceAsStream("/ebi.dll");
					fileOut = new File(System.getProperty("java.io.tmpdir") + "/" + path + ".dll");
                } else {
					// linux
					in = Ebi.class.getResourceAsStream("/libebi.so");
					fileOut = new File(System.getProperty("java.io.tmpdir") + "/" + path + ".so");
                }
				OutputStream out = new FileOutputStream(fileOut);
				in.transferTo(out);
				in.close();
				out.close();
				System.load(fileOut.getAbsolutePath());
				System.out.println("Ebi library loaded from jar.");
				fileOut.deleteOnExit();
            } catch (Exception f) {
				System.out.println("Could not load the Ebi library.");
				f.printStackTrace();
            }
        }
    }

    public static native String callEbi(String command_name, String output_format, String[] inputs);

	public static native String analyseAllTraces(String output_format, String arg0);
	public static native String analyseCohortAnalysis(String output_format, String arg0, String arg1, String arg2);
	public static native String analyseCompleteness(String output_format, String arg0);
	public static native String analyseCoverage(String output_format, String arg0, String arg1);
	public static native String analyseDirectlyFollowsEdgeDifference(String output_format, String arg0, String arg1);
	public static native String analyseDirectlyFollowsEdgeDifferenceNoFrequencies(String output_format, String arg0, String arg1);
	public static native String analyseEntropy(String output_format, String arg0);
	public static native String analyseMedoid(String output_format, String arg0, String arg1);
	public static native String analyseMinimumProbabilityTraces(String output_format, String arg0, String arg1);
	public static native String analyseMode(String output_format, String arg0);
	public static native String analyseMostLikelyTraces(String output_format, String arg0, String arg1);
	public static native String analyseVariety(String output_format, String arg0);
	public static native String analyseNonStochasticActivities(String output_format, String arg0);
	public static native String analyseNonStochasticAnyTraces(String output_format, String arg0);
	public static native String analyseNonStochasticBounded(String output_format, String arg0);
	public static native String analyseNonStochasticCluster(String output_format, String arg0, String arg1);
	public static native String analyseNonStochasticEmptyTraces(String output_format, String arg0);
	public static native String analyseNonStochasticExecutions(String output_format, String arg0, String arg1);
	public static native String analyseNonStochasticInfinitelyManyTraces(String output_format, String arg0);
	public static native String analyseNonStochasticMedoid(String output_format, String arg0, String arg1);
	public static native String analyseNonStochasticTimestampsOrdered(String output_format, String arg0);
	public static native String associationAllTraceAttributes(String output_format, String arg0, String arg1);
	public static native String associationTraceAttribute(String output_format, String arg0, String arg1, String arg2);
	public static native String conformanceChiSquared(String output_format, String arg0, String arg1);
	public static native String conformanceChiSquaredSample(String output_format, String arg0, String arg1, String arg2);
	public static native String conformanceEarthMovers(String output_format, String arg0, String arg1);
	public static native String conformanceEarthMoversSample(String output_format, String arg0, String arg1, String arg2);
	public static native String conformanceEntropicRelevance(String output_format, String arg0, String arg1);
	public static native String conformanceGainPrecision(String output_format, String arg0, String arg1, String arg2);
	public static native String conformanceGainRecall(String output_format, String arg0, String arg1, String arg2);
	public static native String conformanceHellinger(String output_format, String arg0, String arg1);
	public static native String conformanceHellingerSample(String output_format, String arg0, String arg1, String arg2);
	public static native String conformanceJensenShannon(String output_format, String arg0, String arg1);
	public static native String conformanceJensenShannonSample(String output_format, String arg0, String arg1, String arg2);
	public static native String conformanceMarkovian(String output_format, String arg0, String arg1, String arg2, String arg3);
	public static native String conformanceUnitEarthMovers(String output_format, String arg0, String arg1);
	public static native String conformanceUnitEarthMoversSample(String output_format, String arg0, String arg1, String arg2);
	public static native String conformanceNonStochasticAlignments(String output_format, String arg0, String arg1, String arg2);
	public static native String conformanceNonStochasticEscapingEdgesPrecision(String output_format, String arg0, String arg1);
	public static native String conformanceNonStochasticFittingTraces(String output_format, String arg0);
	public static native String conformanceNonStochasticSetAlignments(String output_format, String arg0, String arg1);
	public static native String conformanceNonStochasticTraceFitness(String output_format, String arg0);
	public static native String convertBusinessProcessModelAndNotation(String output_format, String arg0);
	public static native String convertFiniteLanguage(String output_format, String arg0);
	public static native String convertFiniteStochasticLanguage(String output_format, String arg0);
	public static native String convertLabelledPetriNet(String output_format, String arg0);
	public static native String convertLog(String output_format, String arg0);
	public static native String convertStochasticDeterministicFiniteAutomaton(String output_format, String arg0);
	public static native String convertStochasticDirectlyFollowsModel(String output_format, String arg0);
	public static native String convertStochasticLabelledPetriNet(String output_format, String arg0);
	public static native String convertStochasticNondeterministicFiniteAutomaton(String output_format, String arg0);
	public static native String discoverAlergia(String output_format, String arg0, String arg1);
	public static native String discoverAlignmentsStochasticBusinessProcessModelAndNotation(String output_format, String arg0, String arg1);
	public static native String discoverAlignmentsStochasticLabelledPetriNets(String output_format, String arg0, String arg1);
	public static native String discoverCanItBeDoneOptimally(String output_format, String arg0, String arg1);
	public static native String discoverDirectlyFollowsGraph(String output_format, String arg0, String arg1);
	public static native String discoverOccurrenceStochasticBusinessProcessModelAndNotation(String output_format, String arg0, String arg1);
	public static native String discoverOccurrenceStochasticLabelledPetriNet(String output_format, String arg0, String arg1);
	public static native String discoverOccurrenceStochasticProcessTree(String output_format, String arg0, String arg1);
	public static native String discoverRandomStochasticBusinessProcessModelAndNotation(String output_format, String arg0);
	public static native String discoverRandomStochasticLabelledPetriNet(String output_format, String arg0);
	public static native String discoverRandomStochasticProcessTree(String output_format, String arg0);
	public static native String discoverUniformStochasticBusinessProcessModelAndNotation(String output_format, String arg0);
	public static native String discoverUniformStochasticLabelledPetriNet(String output_format, String arg0);
	public static native String discoverUniformStochasticProcessTree(String output_format, String arg0);
	public static native String discoverNonStochasticFlowerDeterministicFiniteAutomaton(String output_format, String arg0);
	public static native String discoverNonStochasticFlowerProcessTree(String output_format, String arg0);
	public static native String discoverNonStochasticInductiveMiner(String output_format, String arg0);
	public static native String discoverNonStochasticInductiveMinerInfrequent(String output_format, String arg0, String arg1);
	public static native String discoverNonStochasticPrefixTreeDeterministicFiniteAutomaton(String output_format, String arg0);
	public static native String discoverNonStochasticPrefixTreeProcessTree(String output_format, String arg0);
	public static native String discoverNonStochasticSplitMiner(String output_format, String arg0, String arg1, String arg2);
	public static native String discoverNonStochasticTraceModel(String output_format, String arg0);
	public static native String filterLanguageTracesEmpty(String output_format, String arg0);
	public static native String filterLanguageTracesEventActivity(String output_format, String arg0, String arg1, String arg2);
	public static native String filterLanguageTracesLength(String output_format, String arg0, String arg1, String arg2);
	public static native String filterLogTracesEmpty(String output_format, String arg0);
	public static native String filterLogTracesEventActivity(String output_format, String arg0, String arg1, String arg2);
	public static native String filterLogTracesLength(String output_format, String arg0, String arg1, String arg2);
	public static native String filterStochasticLanguageTracesEmpty(String output_format, String arg0);
	public static native String filterStochasticLanguageTracesEventActivity(String output_format, String arg0, String arg1, String arg2);
	public static native String filterStochasticLanguageTracesLeastLikely(String output_format, String arg0, String arg1);
	public static native String filterStochasticLanguageTracesLength(String output_format, String arg0, String arg1, String arg2);
	public static native String filterStochasticLanguageTracesMostLikely(String output_format, String arg0, String arg1);
	public static native String information(String output_format, String arg0);
	public static native String probabilityLog(String output_format, String arg0, String arg1);
	public static native String reduceLabelledPetriNet(String output_format, String arg0);
	public static native String reduceProcessTree(String output_format, String arg0);
	public static native String sampleFolds(String output_format, String arg0, String arg1, String arg2, String arg3);
	public static native String samplePartiallyOrderedTraces(String output_format, String arg0, String arg1);
	public static native String sampleTraces(String output_format, String arg0, String arg1);
	public static native String testBootstrapTest(String output_format, String arg0, String arg1, String arg2, String arg3);
	public static native String testLogCategoricalAttribute(String output_format, String arg0, String arg1, String arg2, String arg3);
	public static native String testPermutationTestLogModel(String output_format, String arg0, String arg1, String arg2, String arg3);
	public static native String visualiseGraph(String output_format, String arg0);
	public static native String visualiseText(String output_format, String arg0);

}
