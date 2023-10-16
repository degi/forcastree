# COLOR_DARK <- "#6c584c"
# COLOR_LIGHT <- "#F0EAD2"

parameters <- list(
  "Species description" = list(
    title = "Species name and description",
    fields = c("Label", "Description"),
    headers = c("Species", "Description")),
  
  "Growth rate" = list(
    title = "Growth rate of Diameter at Breast Height (DBH)",
    fields = c("DbhInitial", "DbhMaximum", "DbhK",	"DbhC"),
    headers = c("Initial DBH (cm)", "Max DBH (cm)", "k",	"c"),
    display_scale = c(100, 100, NA, NA)),
  
  "Height allometry" = list(
    title = "Height maximum and allometric coeficients",
    fields = c("HeigthMaxAbsolute", "HeightA", "HeightB"),
    headers = c("Max Height (m)", "alpha", "beta")),
  
  "Tree crown" = list(
    title = "Tree crown properties and allometric coeficients",
    fields = c("CrownPorosity", "CrownShyness", "CrownWidthA",	"CrownWidthB"),
    headers = c("Crown Porosity", "Crown Shyness", "a", "b")),
  
  "Light sensitivity" = list(
    title = "Light requirement and sensitivity",
    fields = c("LightMininum", "LightOptimum", "LightSensiTree", 
               "LightFlexiTree","LightSensiCrown", "LightFlexiCrown"),
    headers = c("Minimum (%)", "Optimum (%)", "Tree Sensi", "Tree Flexi",
                "Crown Sensi", "Crown Flexi"),
    display_scale = c(100, 100, NA, NA, NA, NA)),
  
  "Mortality" = list(
    title = "Survival and mortality",
    fields = c("SurvivalProbability", "MortalityModifier", "Mortality2thProbability"),
    headers = c("Survival Probability", "Mortality Modifier", "Mortality 2th Probability")),
  
  "Regeneration" = list(
    title = "Regeneration",
    fields = c("AdultSize", "RegenerationBetaShape1", "RegenerationBetaShape2",
               "RegenerationMetaComposition", "RegenerationRelativeAbundance"),
    headers = c("Adult DBH (cm)", "Beta Shape 1", "Beta Shape 2",
                "Meta Composition", "Relative Abundance")),
  
  "Belowground" = list(
    title = "Root and belowground",
    fields = c("RootZoneModifier", "ImperataCompetitionFactor",
               "IndexOfRootAnchoring", "IndexOfRootBinding"),
    headers = c("Root Zone Modifier", "Imperata Competition Factor",
                "Index Of Root Anchoring", "Index Of Root Binding")),
  
  "Wood and stem" = list(
    title = "Wood density and tree tapping",
    fields = c("WoodDensity", "Tapped", "MinTappingSize"),
    headers = c("Wood Density", "Is Tapped", "Minimum Tapping Size (cm)"),
    render = c(NA, "checkbox", NA))
)

get_params_groups <- function() {
  return(names(parameters))
}
