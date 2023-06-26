from rdflib import RDF, Literal, Graph, URIRef
from ontology.namespaces_definition import *


class BIGGObjects(object):
    __rdf_type__ = "__invalid__"
    __rdf_namespace__ = "__invalid__"

    @classmethod
    def set_namespace(cls, namespace):
        cls.__rdf_namespace__ = namespace

    def __init__(self, subject):
        self.subject = self.__rdf_namespace__[subject]
        if self.__class__ == BIGGObjects.__class__:
            raise NotImplementedError("The BiggObject is abstract")

    def get_graph(self):
        g = Graph()
        for tt in self.__rdf_type__:
            g.add((self.subject, RDF.type, eval(tt)))
        for k, v in vars(self).items():
            if k != "subject" and v:
                if isinstance(v, URIRef):
                    g.add((self.subject, Bigg[k], v))
                else:
                    g.add((self.subject, Bigg[k], Literal(v)))
        return g
    
        
class AddressCity(BIGGObjects):
    __rdf_type__ = ['Bigg.AddressCity', 'Bigg.Feature', 'Bigg.Thing']

    def __init__(self, subject, name=None,comment=None,label=None):
        super().__init__(subject)
        self.name = name
        self.comment = comment
        self.label = label
        
        
class AddressCountry(BIGGObjects):
    __rdf_type__ = ['Bigg.AddressCountry', 'Bigg.Feature', 'Bigg.Thing']

    def __init__(self, subject, name=None,comment=None,label=None):
        super().__init__(subject)
        self.name = name
        self.comment = comment
        self.label = label
        
        
class AddressProvince(BIGGObjects):
    __rdf_type__ = ['Bigg.AddressProvince', 'Bigg.Feature', 'Bigg.Thing']

    def __init__(self, subject, name=None,comment=None,label=None):
        super().__init__(subject)
        self.name = name
        self.comment = comment
        self.label = label
        
        
class EnergyPerformanceContract(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergyPerformanceContract', 'Bigg.Contract', 'Bigg.Thing']

    def __init__(self, subject, contractEndDate=None,contractName=None,contractPerimeter=None,contractStartDate=None,comment=None,label=None,hasObjective=None,providesContract=None):
        super().__init__(subject)
        self.contractEndDate = contractEndDate
        self.contractName = contractName
        self.contractPerimeter = contractPerimeter
        self.contractStartDate = contractStartDate
        self.comment = comment
        self.label = label
        self.hasObjective = hasObjective
        self.providesContract = providesContract
        
        
class Building(BIGGObjects):
    __rdf_type__ = ['Bigg.Building', 'Bigg.KPICalculationItem', 'Bigg.Thing']

    def __init__(self, subject, buildingClosingHour=None,buildingConstructionYear=None,buildingIDFromOrganization=None,buildingName=None,buildingOpeningHour=None,comment=None,label=None,hasBuildingConstructionType=None,hasBuildingOwnership=None,hasCadastralInfo=None,hasLocationInfo=None,pertainsToOrganization=None,hasSpace=None,assessesSingleKPI=None,groupsForAnalytics=None,hasAnalyticalModel=None,hasKPI=None,hasProject=None,hasEPC=None):
        super().__init__(subject)
        self.buildingClosingHour = buildingClosingHour
        self.buildingConstructionYear = buildingConstructionYear
        self.buildingIDFromOrganization = buildingIDFromOrganization
        self.buildingName = buildingName
        self.buildingOpeningHour = buildingOpeningHour
        self.comment = comment
        self.label = label
        self.hasBuildingConstructionType = hasBuildingConstructionType
        self.hasBuildingOwnership = hasBuildingOwnership
        self.hasCadastralInfo = hasCadastralInfo
        self.hasLocationInfo = hasLocationInfo
        self.pertainsToOrganization = pertainsToOrganization
        self.hasSpace = hasSpace
        self.assessesSingleKPI = assessesSingleKPI
        self.groupsForAnalytics = groupsForAnalytics
        self.hasAnalyticalModel = hasAnalyticalModel
        self.hasKPI = hasKPI
        self.hasProject = hasProject
        self.hasEPC = hasEPC
        
        
class Element(BIGGObjects):
    __rdf_type__ = ['Bigg.Element', 'Bigg.ObservableItem', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,isObservedByDevice=None,hasSubElement=None,isAffectedByMeasure=None,isAssociatedWithSpace=None,isContainedInSpace=None,maintainsElement=None,isContainedInSystem=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.isObservedByDevice = isObservedByDevice
        self.hasSubElement = hasSubElement
        self.isAffectedByMeasure = isAffectedByMeasure
        self.isAssociatedWithSpace = isAssociatedWithSpace
        self.isContainedInSpace = isContainedInSpace
        self.maintainsElement = maintainsElement
        self.isContainedInSystem = isContainedInSystem
        
        
class Measurement(BIGGObjects):
    __rdf_type__ = ['Bigg.Measurement', 'Bigg.TimeSeriesPoint', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,end=None,isReal=None,start=None,value=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.end = end
        self.isReal = isReal
        self.start = start
        self.value = value
        
        
class StatePoint(BIGGObjects):
    __rdf_type__ = ['Bigg.StatePoint', 'Bigg.TimeSeriesPoint', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,end=None,isReal=None,start=None,value=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.end = end
        self.isReal = isReal
        self.start = start
        self.value = value
        
        
class WeatherStation(BIGGObjects):
    __rdf_type__ = ['Bigg.WeatherStation', 'Bigg.DataProvider', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,latitude=None,longitude=None,hasDeviceInputProtocol=None,hasDeviceInputSignalType=None,hasDeviceLocationInfo=None,hasDeviceType=None,hasHistory=None,hasSensor=None,hasState=None,isPartOfDeviceAggregator=None,observes=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.latitude = latitude
        self.longitude = longitude
        self.hasDeviceInputProtocol = hasDeviceInputProtocol
        self.hasDeviceInputSignalType = hasDeviceInputSignalType
        self.hasDeviceLocationInfo = hasDeviceLocationInfo
        self.hasDeviceType = hasDeviceType
        self.hasHistory = hasHistory
        self.hasSensor = hasSensor
        self.hasState = hasState
        self.isPartOfDeviceAggregator = isPartOfDeviceAggregator
        self.observes = observes
        
        
class State(BIGGObjects):
    __rdf_type__ = ['Bigg.State', 'Bigg.TimeSeriesList', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,timeSeriesEnd=None,timeSeriesFrequency=None,timeSeriesIsCumulative=None,timeSeriesIsOnChange=None,timeSeriesIsRegular=None,timeSeriesStart=None,timeSeriesTimeAggregationFunction=None,hasEstimationMethod=None,hasMeasuredProperty=None,hasStatePoint=None,hasStateType=None,hasStateUnit=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.timeSeriesEnd = timeSeriesEnd
        self.timeSeriesFrequency = timeSeriesFrequency
        self.timeSeriesIsCumulative = timeSeriesIsCumulative
        self.timeSeriesIsOnChange = timeSeriesIsOnChange
        self.timeSeriesIsRegular = timeSeriesIsRegular
        self.timeSeriesStart = timeSeriesStart
        self.timeSeriesTimeAggregationFunction = timeSeriesTimeAggregationFunction
        self.hasEstimationMethod = hasEstimationMethod
        self.hasMeasuredProperty = hasMeasuredProperty
        self.hasStatePoint = hasStatePoint
        self.hasStateType = hasStateType
        self.hasStateUnit = hasStateUnit
        
        
class Sensor(BIGGObjects):
    __rdf_type__ = ['Bigg.Sensor', 'Bigg.TimeSeriesList', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,timeSeriesEnd=None,timeSeriesFrequency=None,timeSeriesIsCumulative=None,timeSeriesIsOnChange=None,timeSeriesIsRegular=None,timeSeriesStart=None,timeSeriesTimeAggregationFunction=None,hasEstimationMethod=None,hasMeasuredProperty=None,hasMeasurement=None,hasMeasurementUnit=None,hasOutputProtocol=None,hasOutputSignalType=None,hasSensorReadingType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.timeSeriesEnd = timeSeriesEnd
        self.timeSeriesFrequency = timeSeriesFrequency
        self.timeSeriesIsCumulative = timeSeriesIsCumulative
        self.timeSeriesIsOnChange = timeSeriesIsOnChange
        self.timeSeriesIsRegular = timeSeriesIsRegular
        self.timeSeriesStart = timeSeriesStart
        self.timeSeriesTimeAggregationFunction = timeSeriesTimeAggregationFunction
        self.hasEstimationMethod = hasEstimationMethod
        self.hasMeasuredProperty = hasMeasuredProperty
        self.hasMeasurement = hasMeasurement
        self.hasMeasurementUnit = hasMeasurementUnit
        self.hasOutputProtocol = hasOutputProtocol
        self.hasOutputSignalType = hasOutputSignalType
        self.hasSensorReadingType = hasSensorReadingType
        
        
class BuildingSpace(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingSpace', 'Bigg.ObservableItem', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,buildingSpaceIDFromOrganization=None,buildingSpaceName=None,hasDeviceAggregator=None,hasSubSpace=None,hasUtilityPointOfDelivery=None,isObservedByDevice=None,containsElement=None,hasArea=None,hasBuildingSpaceUseType=None,hasIndoorQualityPerception=None,hasOccupancyProfile=None,isAssociatedWithElement=None,isContainedInZone=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.buildingSpaceIDFromOrganization = buildingSpaceIDFromOrganization
        self.buildingSpaceName = buildingSpaceName
        self.hasDeviceAggregator = hasDeviceAggregator
        self.hasSubSpace = hasSubSpace
        self.hasUtilityPointOfDelivery = hasUtilityPointOfDelivery
        self.isObservedByDevice = isObservedByDevice
        self.containsElement = containsElement
        self.hasArea = hasArea
        self.hasBuildingSpaceUseType = hasBuildingSpaceUseType
        self.hasIndoorQualityPerception = hasIndoorQualityPerception
        self.hasOccupancyProfile = hasOccupancyProfile
        self.isAssociatedWithElement = isAssociatedWithElement
        self.isContainedInZone = isContainedInZone
        
        
class Device(BIGGObjects):
    __rdf_type__ = ['Bigg.Device', 'Bigg.DataProvider', 'Bigg.Element', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,contractedPower=None,deviceIDFromOrganization=None,deviceInstallationDate=None,deviceLicenceVersionNumber=None,deviceManufacturer=None,deviceModel=None,deviceName=None,deviceNumberOfOutputs=None,deviceOperatingSystem=None,deviceRemovalDate=None,deviceSerialNumber=None,deviceThresholdValue=None,distributor=None,maxPowerAPM=None,maxPowerBEI=None,renoteManagement=None,selfConsumption=None,tariff=None,tension=None,hasDeviceInputProtocol=None,hasDeviceInputSignalType=None,hasDeviceLocationInfo=None,hasDeviceType=None,hasHistory=None,hasSensor=None,hasState=None,isPartOfDeviceAggregator=None,observes=None,hasSubElement=None,isAffectedByMeasure=None,isAssociatedWithSpace=None,isContainedInSpace=None,maintainsElement=None,isContainedInSystem=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.contractedPower = contractedPower
        self.deviceIDFromOrganization = deviceIDFromOrganization
        self.deviceInstallationDate = deviceInstallationDate
        self.deviceLicenceVersionNumber = deviceLicenceVersionNumber
        self.deviceManufacturer = deviceManufacturer
        self.deviceModel = deviceModel
        self.deviceName = deviceName
        self.deviceNumberOfOutputs = deviceNumberOfOutputs
        self.deviceOperatingSystem = deviceOperatingSystem
        self.deviceRemovalDate = deviceRemovalDate
        self.deviceSerialNumber = deviceSerialNumber
        self.deviceThresholdValue = deviceThresholdValue
        self.distributor = distributor
        self.maxPowerAPM = maxPowerAPM
        self.maxPowerBEI = maxPowerBEI
        self.renoteManagement = renoteManagement
        self.selfConsumption = selfConsumption
        self.tariff = tariff
        self.tension = tension
        self.hasDeviceInputProtocol = hasDeviceInputProtocol
        self.hasDeviceInputSignalType = hasDeviceInputSignalType
        self.hasDeviceLocationInfo = hasDeviceLocationInfo
        self.hasDeviceType = hasDeviceType
        self.hasHistory = hasHistory
        self.hasSensor = hasSensor
        self.hasState = hasState
        self.isPartOfDeviceAggregator = isPartOfDeviceAggregator
        self.observes = observes
        self.hasSubElement = hasSubElement
        self.isAffectedByMeasure = isAffectedByMeasure
        self.isAssociatedWithSpace = isAssociatedWithSpace
        self.isContainedInSpace = isContainedInSpace
        self.maintainsElement = maintainsElement
        self.isContainedInSystem = isContainedInSystem
        
        
class BuildingConstructionElement(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingConstructionElement', 'Bigg.BuildingElement', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,buildingElementBrand=None,buildingElementIdFromOrganizationstring=None,buildingElementInstallationDate=None,buildingElementManufactureDatestring=None,buildingElementManufacturer=None,buildingElementModelstring=None,buildingElementPurchaseDate=None,buildingElementSerialNumber=None,buildingElementState=None,hasBuildingConstructionElementType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.buildingElementBrand = buildingElementBrand
        self.buildingElementIdFromOrganizationstring = buildingElementIdFromOrganizationstring
        self.buildingElementInstallationDate = buildingElementInstallationDate
        self.buildingElementManufactureDatestring = buildingElementManufactureDatestring
        self.buildingElementManufacturer = buildingElementManufacturer
        self.buildingElementModelstring = buildingElementModelstring
        self.buildingElementPurchaseDate = buildingElementPurchaseDate
        self.buildingElementSerialNumber = buildingElementSerialNumber
        self.buildingElementState = buildingElementState
        self.hasBuildingConstructionElementType = hasBuildingConstructionElementType
        
        
class BuildingSystemElement(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingSystemElement', 'Bigg.BuildingElement', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,buildingElementBrand=None,buildingElementIdFromOrganizationstring=None,buildingElementInstallationDate=None,buildingElementManufactureDatestring=None,buildingElementManufacturer=None,buildingElementModelstring=None,buildingElementPurchaseDate=None,buildingElementSerialNumber=None,buildingElementState=None,buildingSystemElementEfficiency=None,buildingSystemElementMaxOutput=None,buildingSystemElementMinOutput=None,hasBuildingSystemElementType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.buildingElementBrand = buildingElementBrand
        self.buildingElementIdFromOrganizationstring = buildingElementIdFromOrganizationstring
        self.buildingElementInstallationDate = buildingElementInstallationDate
        self.buildingElementManufactureDatestring = buildingElementManufactureDatestring
        self.buildingElementManufacturer = buildingElementManufacturer
        self.buildingElementModelstring = buildingElementModelstring
        self.buildingElementPurchaseDate = buildingElementPurchaseDate
        self.buildingElementSerialNumber = buildingElementSerialNumber
        self.buildingElementState = buildingElementState
        self.buildingSystemElementEfficiency = buildingSystemElementEfficiency
        self.buildingSystemElementMaxOutput = buildingSystemElementMaxOutput
        self.buildingSystemElementMinOutput = buildingSystemElementMinOutput
        self.hasBuildingSystemElementType = hasBuildingSystemElementType
        
        
class BuildingElement(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingElement', 'Bigg.Element', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,buildingElementBrand=None,buildingElementIdFromOrganizationstring=None,buildingElementInstallationDate=None,buildingElementManufactureDatestring=None,buildingElementManufacturer=None,buildingElementModelstring=None,buildingElementPurchaseDate=None,buildingElementSerialNumber=None,buildingElementState=None,hasSubElement=None,isAffectedByMeasure=None,isAssociatedWithSpace=None,isContainedInSpace=None,maintainsElement=None,isContainedInSystem=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.buildingElementBrand = buildingElementBrand
        self.buildingElementIdFromOrganizationstring = buildingElementIdFromOrganizationstring
        self.buildingElementInstallationDate = buildingElementInstallationDate
        self.buildingElementManufactureDatestring = buildingElementManufactureDatestring
        self.buildingElementManufacturer = buildingElementManufacturer
        self.buildingElementModelstring = buildingElementModelstring
        self.buildingElementPurchaseDate = buildingElementPurchaseDate
        self.buildingElementSerialNumber = buildingElementSerialNumber
        self.buildingElementState = buildingElementState
        self.hasSubElement = hasSubElement
        self.isAffectedByMeasure = isAffectedByMeasure
        self.isAssociatedWithSpace = isAssociatedWithSpace
        self.isContainedInSpace = isContainedInSpace
        self.maintainsElement = maintainsElement
        self.isContainedInSystem = isContainedInSystem
        
        
class EnergyEfficiencyMeasure(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergyEfficiencyMeasure', 'Bigg.KPICalculationItem', 'Bigg.NonEnergyBenefitProducingItem', 'Bigg.SavingProducingItem', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,energyEfficiencyMeasureCO2Reduction=None,energyEfficiencyMeasureCurrencyExchangeRate=None,energyEfficiencyMeasureDescription=None,energyEfficiencyMeasureFinancialSavings=None,energyEfficiencyMeasureInvestment=None,energyEfficiencyMeasureLifetime=None,energyEfficiencyMeasureOperationalDate=None,energyEfficiencyMeasureSavingsToInvestmentRatio=None,energySourcePriceEscalationRate=None,shareOfAffectedElement=None,affectsElement=None,hasEnergyEfficiencyMeasureInvestmentCurrency=None,hasEnergyEfficiencyMeasureType=None,assessesSingleKPI=None,groupsForAnalytics=None,hasAnalyticalModel=None,hasKPI=None,producesNonEnergyBenefit=None,producesSaving=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.energyEfficiencyMeasureCO2Reduction = energyEfficiencyMeasureCO2Reduction
        self.energyEfficiencyMeasureCurrencyExchangeRate = energyEfficiencyMeasureCurrencyExchangeRate
        self.energyEfficiencyMeasureDescription = energyEfficiencyMeasureDescription
        self.energyEfficiencyMeasureFinancialSavings = energyEfficiencyMeasureFinancialSavings
        self.energyEfficiencyMeasureInvestment = energyEfficiencyMeasureInvestment
        self.energyEfficiencyMeasureLifetime = energyEfficiencyMeasureLifetime
        self.energyEfficiencyMeasureOperationalDate = energyEfficiencyMeasureOperationalDate
        self.energyEfficiencyMeasureSavingsToInvestmentRatio = energyEfficiencyMeasureSavingsToInvestmentRatio
        self.energySourcePriceEscalationRate = energySourcePriceEscalationRate
        self.shareOfAffectedElement = shareOfAffectedElement
        self.affectsElement = affectsElement
        self.hasEnergyEfficiencyMeasureInvestmentCurrency = hasEnergyEfficiencyMeasureInvestmentCurrency
        self.hasEnergyEfficiencyMeasureType = hasEnergyEfficiencyMeasureType
        self.assessesSingleKPI = assessesSingleKPI
        self.groupsForAnalytics = groupsForAnalytics
        self.hasAnalyticalModel = hasAnalyticalModel
        self.hasKPI = hasKPI
        self.producesNonEnergyBenefit = producesNonEnergyBenefit
        self.producesSaving = producesSaving
        
        
class BuildingKeyPerformanceIndicator(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingKeyPerformanceIndicator', 'Bigg.KeyPerformanceIndicator', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,hasKPIType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.hasKPIType = hasKPIType
        
        
class EEMKeyPerformanceIndicator(BIGGObjects):
    __rdf_type__ = ['Bigg.EEMKeyPerformanceIndicator', 'Bigg.KeyPerformanceIndicator', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,hasKPIType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.hasKPIType = hasKPIType
        
        
class SingleKPIAssessmentPoint(BIGGObjects):
    __rdf_type__ = ['Bigg.SingleKPIAssessmentPoint', 'Bigg.TimeSeriesPoint', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,end=None,isReal=None,start=None,value=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.end = end
        self.isReal = isReal
        self.start = start
        self.value = value
        
        
class AggregatedKPIAssessment(BIGGObjects):
    __rdf_type__ = ['Bigg.AggregatedKPIAssessment', 'Bigg.KPIAssessment', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,hasAggregatedKPIPoint=None,hasAggregationFunction=None,hasKPIUnit=None,isEstimatedByModel=None,quantifiesKPI=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.hasAggregatedKPIPoint = hasAggregatedKPIPoint
        self.hasAggregationFunction = hasAggregationFunction
        self.hasKPIUnit = hasKPIUnit
        self.isEstimatedByModel = isEstimatedByModel
        self.quantifiesKPI = quantifiesKPI
        
        
class AggregatedKPIAssessmentPoint(BIGGObjects):
    __rdf_type__ = ['Bigg.AggregatedKPIAssessmentPoint', 'Bigg.TimeSeriesPoint', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,end=None,isReal=None,start=None,value=None,hasAggregationFunction=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.end = end
        self.isReal = isReal
        self.start = start
        self.value = value
        self.hasAggregationFunction = hasAggregationFunction
        
        
class SingleKPIAssessment(BIGGObjects):
    __rdf_type__ = ['Bigg.SingleKPIAssessment', 'Bigg.KPIAssessment', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,hasKPIUnit=None,hasSingleKPIPoint=None,isEstimatedByModel=None,quantifiesKPI=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.hasKPIUnit = hasKPIUnit
        self.hasSingleKPIPoint = hasSingleKPIPoint
        self.isEstimatedByModel = isEstimatedByModel
        self.quantifiesKPI = quantifiesKPI
        
        
class KPIAssessment(BIGGObjects):
    __rdf_type__ = ['Bigg.KPIAssessment', 'Bigg.TimeSeriesList', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,timeSeriesEnd=None,timeSeriesFrequency=None,timeSeriesIsCumulative=None,timeSeriesIsOnChange=None,timeSeriesIsRegular=None,timeSeriesStart=None,timeSeriesTimeAggregationFunction=None,hasEstimationMethod=None,hasMeasuredProperty=None,hasKPIUnit=None,isEstimatedByModel=None,quantifiesKPI=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.timeSeriesEnd = timeSeriesEnd
        self.timeSeriesFrequency = timeSeriesFrequency
        self.timeSeriesIsCumulative = timeSeriesIsCumulative
        self.timeSeriesIsOnChange = timeSeriesIsOnChange
        self.timeSeriesIsRegular = timeSeriesIsRegular
        self.timeSeriesStart = timeSeriesStart
        self.timeSeriesTimeAggregationFunction = timeSeriesTimeAggregationFunction
        self.hasEstimationMethod = hasEstimationMethod
        self.hasMeasuredProperty = hasMeasuredProperty
        self.hasKPIUnit = hasKPIUnit
        self.isEstimatedByModel = isEstimatedByModel
        self.quantifiesKPI = quantifiesKPI
        
        
class RenovationProject(BIGGObjects):
    __rdf_type__ = ['Bigg.RenovationProject', 'Bigg.Project', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,geometrySRID=None,projectCurrencyExchangeRate=None,projectDescription=None,projectDiscountRate=None,projectGrantsShareOfCosts=None,projectIDFromOrganization=None,projectIncludedConfortmeterSurvey=None,projectIncludedNonEnergyBenefitsEstimate=None,projectInterestRate=None,projectInternalRateOfReturn=None,projectInventivesShareOfRevenues=None,projectInvestment=None,projectName=None,projectNetPresentValue=None,projectOperationalDate=None,projectReceivedGrantFounding=None,projectSavingsToInvestmentRatio=None,projectSimplePaybackTime=None,projectStartDate=None,projectUsesIncentives=None,affectsBuilding=None,hasProjectInvestmentCurrency=None,hasProjectMotivation=None,hasSubProject=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.geometrySRID = geometrySRID
        self.projectCurrencyExchangeRate = projectCurrencyExchangeRate
        self.projectDescription = projectDescription
        self.projectDiscountRate = projectDiscountRate
        self.projectGrantsShareOfCosts = projectGrantsShareOfCosts
        self.projectIDFromOrganization = projectIDFromOrganization
        self.projectIncludedConfortmeterSurvey = projectIncludedConfortmeterSurvey
        self.projectIncludedNonEnergyBenefitsEstimate = projectIncludedNonEnergyBenefitsEstimate
        self.projectInterestRate = projectInterestRate
        self.projectInternalRateOfReturn = projectInternalRateOfReturn
        self.projectInventivesShareOfRevenues = projectInventivesShareOfRevenues
        self.projectInvestment = projectInvestment
        self.projectName = projectName
        self.projectNetPresentValue = projectNetPresentValue
        self.projectOperationalDate = projectOperationalDate
        self.projectReceivedGrantFounding = projectReceivedGrantFounding
        self.projectSavingsToInvestmentRatio = projectSavingsToInvestmentRatio
        self.projectSimplePaybackTime = projectSimplePaybackTime
        self.projectStartDate = projectStartDate
        self.projectUsesIncentives = projectUsesIncentives
        self.affectsBuilding = affectsBuilding
        self.hasProjectInvestmentCurrency = hasProjectInvestmentCurrency
        self.hasProjectMotivation = hasProjectMotivation
        self.hasSubProject = hasSubProject
        
        
class RetrofitProject(BIGGObjects):
    __rdf_type__ = ['Bigg.RetrofitProject', 'Bigg.Project', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,geometrySRID=None,projectCO2Reduction=None,projectCurrencyExchangeRate=None,projectDescription=None,projectDiscountRate=None,projectGrantsShareOfCosts=None,projectIDFromOrganization=None,projectIncludedConfortmeterSurvey=None,projectIncludedNonEnergyBenefitsEstimate=None,projectInterestRate=None,projectInternalRateOfReturn=None,projectInventivesShareOfRevenues=None,projectInvestment=None,projectName=None,projectNetPresentValue=None,projectOperationalDate=None,projectReceivedGrantFounding=None,projectSavingsToInvestmentRatio=None,projectSimplePaybackTime=None,projectStartDate=None,projectUsesIncentives=None,affectsBuilding=None,hasProjectInvestmentCurrency=None,hasProjectMotivation=None,hasSubProject=None,includesMeasure=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.geometrySRID = geometrySRID
        self.projectCO2Reduction = projectCO2Reduction
        self.projectCurrencyExchangeRate = projectCurrencyExchangeRate
        self.projectDescription = projectDescription
        self.projectDiscountRate = projectDiscountRate
        self.projectGrantsShareOfCosts = projectGrantsShareOfCosts
        self.projectIDFromOrganization = projectIDFromOrganization
        self.projectIncludedConfortmeterSurvey = projectIncludedConfortmeterSurvey
        self.projectIncludedNonEnergyBenefitsEstimate = projectIncludedNonEnergyBenefitsEstimate
        self.projectInterestRate = projectInterestRate
        self.projectInternalRateOfReturn = projectInternalRateOfReturn
        self.projectInventivesShareOfRevenues = projectInventivesShareOfRevenues
        self.projectInvestment = projectInvestment
        self.projectName = projectName
        self.projectNetPresentValue = projectNetPresentValue
        self.projectOperationalDate = projectOperationalDate
        self.projectReceivedGrantFounding = projectReceivedGrantFounding
        self.projectSavingsToInvestmentRatio = projectSavingsToInvestmentRatio
        self.projectSimplePaybackTime = projectSimplePaybackTime
        self.projectStartDate = projectStartDate
        self.projectUsesIncentives = projectUsesIncentives
        self.affectsBuilding = affectsBuilding
        self.hasProjectInvestmentCurrency = hasProjectInvestmentCurrency
        self.hasProjectMotivation = hasProjectMotivation
        self.hasSubProject = hasSubProject
        self.includesMeasure = includesMeasure
        
        
class Project(BIGGObjects):
    __rdf_type__ = ['Bigg.Project', 'Bigg.NonEnergyBenefitProducingItem', 'Bigg.SavingProducingItem', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,geometrySRID=None,projectCurrencyExchangeRate=None,projectDescription=None,projectDiscountRate=None,projectGrantsShareOfCosts=None,projectIDFromOrganization=None,projectIncludedConfortmeterSurvey=None,projectIncludedNonEnergyBenefitsEstimate=None,projectInterestRate=None,projectInternalRateOfReturn=None,projectInventivesShareOfRevenues=None,projectInvestment=None,projectName=None,projectNetPresentValue=None,projectOperationalDate=None,projectReceivedGrantFounding=None,projectSavingsToInvestmentRatio=None,projectSimplePaybackTime=None,projectStartDate=None,projectUsesIncentives=None,affectsBuilding=None,hasProjectInvestmentCurrency=None,hasProjectMotivation=None,hasSubProject=None,producesNonEnergyBenefit=None,producesSaving=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.geometrySRID = geometrySRID
        self.projectCurrencyExchangeRate = projectCurrencyExchangeRate
        self.projectDescription = projectDescription
        self.projectDiscountRate = projectDiscountRate
        self.projectGrantsShareOfCosts = projectGrantsShareOfCosts
        self.projectIDFromOrganization = projectIDFromOrganization
        self.projectIncludedConfortmeterSurvey = projectIncludedConfortmeterSurvey
        self.projectIncludedNonEnergyBenefitsEstimate = projectIncludedNonEnergyBenefitsEstimate
        self.projectInterestRate = projectInterestRate
        self.projectInternalRateOfReturn = projectInternalRateOfReturn
        self.projectInventivesShareOfRevenues = projectInventivesShareOfRevenues
        self.projectInvestment = projectInvestment
        self.projectName = projectName
        self.projectNetPresentValue = projectNetPresentValue
        self.projectOperationalDate = projectOperationalDate
        self.projectReceivedGrantFounding = projectReceivedGrantFounding
        self.projectSavingsToInvestmentRatio = projectSavingsToInvestmentRatio
        self.projectSimplePaybackTime = projectSimplePaybackTime
        self.projectStartDate = projectStartDate
        self.projectUsesIncentives = projectUsesIncentives
        self.affectsBuilding = affectsBuilding
        self.hasProjectInvestmentCurrency = hasProjectInvestmentCurrency
        self.hasProjectMotivation = hasProjectMotivation
        self.hasSubProject = hasSubProject
        self.producesNonEnergyBenefit = producesNonEnergyBenefit
        self.producesSaving = producesSaving
        
        
class System(BIGGObjects):
    __rdf_type__ = ['Bigg.System', 'Bigg.Group', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,groupName=None,containsElement=None,hasSystemType=None,servesZone=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.groupName = groupName
        self.containsElement = containsElement
        self.hasSystemType = hasSystemType
        self.servesZone = servesZone
        
        
class Zone(BIGGObjects):
    __rdf_type__ = ['Bigg.Zone', 'Bigg.Group', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,groupName=None,containsBuildingSpace=None,hasZoneType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.groupName = groupName
        self.containsBuildingSpace = containsBuildingSpace
        self.hasZoneType = hasZoneType
        
        
class AddressClimateZone(BIGGObjects):
    __rdf_type__ = ['Bigg.AddressClimateZone', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class BuildingConstructionType(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingConstructionType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class BuildingOwnership(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingOwnership', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class LandType(BIGGObjects):
    __rdf_type__ = ['Bigg.LandType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class ObjectiveTargetType(BIGGObjects):
    __rdf_type__ = ['Bigg.ObjectiveTargetType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class ObjectiveTargetUnit(BIGGObjects):
    __rdf_type__ = ['Bigg.ObjectiveTargetUnit', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class OrganiationType(BIGGObjects):
    __rdf_type__ = ['Bigg.OrganiationType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class Contract(BIGGObjects):
    __rdf_type__ = ['Bigg.Contract', 'Bigg.Thing']

    def __init__(self, subject, contractEndDate=None,contractName=None,contractStartDate=None,comment=None,label=None):
        super().__init__(subject)
        self.contractEndDate = contractEndDate
        self.contractName = contractName
        self.contractStartDate = contractStartDate
        self.comment = comment
        self.label = label
        
        
class Feature(BIGGObjects):
    __rdf_type__ = ['Bigg.Feature', 'Bigg.Thing']

    def __init__(self, subject, name=None,comment=None,label=None):
        super().__init__(subject)
        self.name = name
        self.comment = comment
        self.label = label
        
        
class CadastralInfo(BIGGObjects):
    __rdf_type__ = ['Bigg.CadastralInfo', 'Bigg.Thing']

    def __init__(self, subject, landArea=None,landCadastralReference=None,landGeometry=None,landGraphicalArea=None,propertyClass=None,comment=None,label=None,hasLandType=None):
        super().__init__(subject)
        self.landArea = landArea
        self.landCadastralReference = landCadastralReference
        self.landGeometry = landGeometry
        self.landGraphicalArea = landGraphicalArea
        self.propertyClass = propertyClass
        self.comment = comment
        self.label = label
        self.hasLandType = hasLandType
        
        
class EnergyPerformanceContractObjective(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergyPerformanceContractObjective', 'Bigg.Thing']

    def __init__(self, subject, objectiveDeadline=None,objectiveDescription=None,objectiveName=None,objectiveTargetValue=None,comment=None,label=None,hasObjectiveTargetType=None,hasObjectiveTargetUnit=None,IsConnectedToEnergySaving=None):
        super().__init__(subject)
        self.objectiveDeadline = objectiveDeadline
        self.objectiveDescription = objectiveDescription
        self.objectiveName = objectiveName
        self.objectiveTargetValue = objectiveTargetValue
        self.comment = comment
        self.label = label
        self.hasObjectiveTargetType = hasObjectiveTargetType
        self.hasObjectiveTargetUnit = hasObjectiveTargetUnit
        self.IsConnectedToEnergySaving = IsConnectedToEnergySaving
        
        
class Person(BIGGObjects):
    __rdf_type__ = ['Bigg.Person', 'Bigg.Thing']

    def __init__(self, subject, email=None,lastName=None,name=None,userName=None,comment=None,label=None,managesOrganization=None):
        super().__init__(subject)
        self.email = email
        self.lastName = lastName
        self.name = name
        self.userName = userName
        self.comment = comment
        self.label = label
        self.managesOrganization = managesOrganization
        
        
class LocationInfo(BIGGObjects):
    __rdf_type__ = ['Bigg.LocationInfo', 'Bigg.Thing']

    def __init__(self, subject, addressAltitude=None,addressLatitude=None,addressLongitude=None,addressPostalCode=None,addressStreetName=None,addressStreetNumber=None,addressTimeZone=None,comment=None,label=None,hasAddressCity=None,hasAddressClimateZone=None,hasAddressCountry=None,hasAddressProvince=None):
        super().__init__(subject)
        self.addressAltitude = addressAltitude
        self.addressLatitude = addressLatitude
        self.addressLongitude = addressLongitude
        self.addressPostalCode = addressPostalCode
        self.addressStreetName = addressStreetName
        self.addressStreetNumber = addressStreetNumber
        self.addressTimeZone = addressTimeZone
        self.comment = comment
        self.label = label
        self.hasAddressCity = hasAddressCity
        self.hasAddressClimateZone = hasAddressClimateZone
        self.hasAddressCountry = hasAddressCountry
        self.hasAddressProvince = hasAddressProvince
        
        
class Organization(BIGGObjects):
    __rdf_type__ = ['Bigg.Organization', 'Bigg.Thing']

    def __init__(self, subject, organizationDivisionType=None,organizationEmail=None,organizationLocalVAT=None,organizationName=None,organizationTelephoneNumber=None,comment=None,label=None,buysContract=None,hasOrganizationType=None,hasSubOrganization=None,isManagedByPerson=None,managesBuilding=None,organizationContactPerson=None):
        super().__init__(subject)
        self.organizationDivisionType = organizationDivisionType
        self.organizationEmail = organizationEmail
        self.organizationLocalVAT = organizationLocalVAT
        self.organizationName = organizationName
        self.organizationTelephoneNumber = organizationTelephoneNumber
        self.comment = comment
        self.label = label
        self.buysContract = buysContract
        self.hasOrganizationType = hasOrganizationType
        self.hasSubOrganization = hasSubOrganization
        self.isManagedByPerson = isManagedByPerson
        self.managesBuilding = managesBuilding
        self.organizationContactPerson = organizationContactPerson
        
        
class DeviceInputProtocol(BIGGObjects):
    __rdf_type__ = ['Bigg.DeviceInputProtocol', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class DeviceInputSignalType(BIGGObjects):
    __rdf_type__ = ['Bigg.DeviceInputSignalType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class DeviceType(BIGGObjects):
    __rdf_type__ = ['Bigg.DeviceType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class OutputProtocol(BIGGObjects):
    __rdf_type__ = ['Bigg.OutputProtocol', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class OutputSignalType(BIGGObjects):
    __rdf_type__ = ['Bigg.OutputSignalType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class SensorReadingType(BIGGObjects):
    __rdf_type__ = ['Bigg.SensorReadingType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class StateType(BIGGObjects):
    __rdf_type__ = ['Bigg.StateType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class UtilityType(BIGGObjects):
    __rdf_type__ = ['Bigg.UtilityType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class DeviceHistory(BIGGObjects):
    __rdf_type__ = ['Bigg.DeviceHistory', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,containsHistoryDevices=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.containsHistoryDevices = containsHistoryDevices
        
        
class EstimationMethod(BIGGObjects):
    __rdf_type__ = ['Bigg.EstimationMethod', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,considerEstimatedValues=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.considerEstimatedValues = considerEstimatedValues
        
        
class MeasuredProperty(BIGGObjects):
    __rdf_type__ = ['Bigg.MeasuredProperty', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class MeasurementUnit(BIGGObjects):
    __rdf_type__ = ['Bigg.MeasurementUnit', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class ObservableItem(BIGGObjects):
    __rdf_type__ = ['Bigg.ObservableItem', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,isObservedByDevice=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.isObservedByDevice = isObservedByDevice
        
        
class UtilityPointOfDelivery(BIGGObjects):
    __rdf_type__ = ['Bigg.UtilityPointOfDelivery', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,pointOfDeliveryIDFromOrganization=None,hasDevice=None,hasUtilityType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.pointOfDeliveryIDFromOrganization = pointOfDeliveryIDFromOrganization
        self.hasDevice = hasDevice
        self.hasUtilityType = hasUtilityType
        
        
class TimeSeriesPoint(BIGGObjects):
    __rdf_type__ = ['Bigg.TimeSeriesPoint', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,end=None,isReal=None,start=None,value=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.end = end
        self.isReal = isReal
        self.start = start
        self.value = value
        
        
class DeviceAggregator(BIGGObjects):
    __rdf_type__ = ['Bigg.DeviceAggregator', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,deviceAggregatorFormula=None,deviceAggregatorFrequency=None,deviceAggregatorName=None,deviceAggregatorTimeAggregationFunction=None,aggregatesSpace=None,hasDeviceAggregatorProperty=None,includesDevice=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.deviceAggregatorFormula = deviceAggregatorFormula
        self.deviceAggregatorFrequency = deviceAggregatorFrequency
        self.deviceAggregatorName = deviceAggregatorName
        self.deviceAggregatorTimeAggregationFunction = deviceAggregatorTimeAggregationFunction
        self.aggregatesSpace = aggregatesSpace
        self.hasDeviceAggregatorProperty = hasDeviceAggregatorProperty
        self.includesDevice = includesDevice
        
        
class TimeSeriesList(BIGGObjects):
    __rdf_type__ = ['Bigg.TimeSeriesList', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,timeSeriesEnd=None,timeSeriesFrequency=None,timeSeriesIsCumulative=None,timeSeriesIsOnChange=None,timeSeriesIsRegular=None,timeSeriesStart=None,timeSeriesTimeAggregationFunction=None,hasEstimationMethod=None,hasMeasuredProperty=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.timeSeriesEnd = timeSeriesEnd
        self.timeSeriesFrequency = timeSeriesFrequency
        self.timeSeriesIsCumulative = timeSeriesIsCumulative
        self.timeSeriesIsOnChange = timeSeriesIsOnChange
        self.timeSeriesIsRegular = timeSeriesIsRegular
        self.timeSeriesStart = timeSeriesStart
        self.timeSeriesTimeAggregationFunction = timeSeriesTimeAggregationFunction
        self.hasEstimationMethod = hasEstimationMethod
        self.hasMeasuredProperty = hasMeasuredProperty
        
        
class DataProvider(BIGGObjects):
    __rdf_type__ = ['Bigg.DataProvider', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,hasDeviceInputProtocol=None,hasDeviceInputSignalType=None,hasDeviceLocationInfo=None,hasDeviceType=None,hasHistory=None,hasSensor=None,hasState=None,isPartOfDeviceAggregator=None,observes=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.hasDeviceInputProtocol = hasDeviceInputProtocol
        self.hasDeviceInputSignalType = hasDeviceInputSignalType
        self.hasDeviceLocationInfo = hasDeviceLocationInfo
        self.hasDeviceType = hasDeviceType
        self.hasHistory = hasHistory
        self.hasSensor = hasSensor
        self.hasState = hasState
        self.isPartOfDeviceAggregator = isPartOfDeviceAggregator
        self.observes = observes
        
        
class AreaType(BIGGObjects):
    __rdf_type__ = ['Bigg.AreaType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class AreaUnitOfMeasurement(BIGGObjects):
    __rdf_type__ = ['Bigg.AreaUnitOfMeasurement', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class BuildingConstructionElementType(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingConstructionElementType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class BuildingSpaceUseType(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingSpaceUseType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class BuildingSystemElementType(BIGGObjects):
    __rdf_type__ = ['Bigg.BuildingSystemElementType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class EnergyEfficiencyMeasureInvestmentCurrency(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergyEfficiencyMeasureInvestmentCurrency', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class EnergyEfficiencyMeasureType(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergyEfficiencyMeasureType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class IndoorQualityUserPerception(BIGGObjects):
    __rdf_type__ = ['Bigg.IndoorQualityUserPerception', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class MaintenanceActionType(BIGGObjects):
    __rdf_type__ = ['Bigg.MaintenanceActionType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class Area(BIGGObjects):
    __rdf_type__ = ['Bigg.Area', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,areaValue=None,hasAreaType=None,hasAreaUnitOfMeasurement=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.areaValue = areaValue
        self.hasAreaType = hasAreaType
        self.hasAreaUnitOfMeasurement = hasAreaUnitOfMeasurement
        
        
class IndoorQualityPerception(BIGGObjects):
    __rdf_type__ = ['Bigg.IndoorQualityPerception', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,indoorQualityValidityEndDate=None,indoorQualityValidityStartDate=None,hasIndoorQualityUserPerception=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.indoorQualityValidityEndDate = indoorQualityValidityEndDate
        self.indoorQualityValidityStartDate = indoorQualityValidityStartDate
        self.hasIndoorQualityUserPerception = hasIndoorQualityUserPerception
        
        
class OccupancyProfile(BIGGObjects):
    __rdf_type__ = ['Bigg.OccupancyProfile', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,occupancyNumberOfOccupants=None,occupancyProfileValidityEndDate=None,occupancyProfileValidityStartDate=None,occupancyVacationDates=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.occupancyNumberOfOccupants = occupancyNumberOfOccupants
        self.occupancyProfileValidityEndDate = occupancyProfileValidityEndDate
        self.occupancyProfileValidityStartDate = occupancyProfileValidityStartDate
        self.occupancyVacationDates = occupancyVacationDates
        
        
class MaintenanceAction(BIGGObjects):
    __rdf_type__ = ['Bigg.MaintenanceAction', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,maintenanceActionDate=None,maintenanceActionDescription=None,maintenanceActionFrequency=None,maintenanceActionIsPeriodic=None,maintenanceActionName=None,hasMaintenanceActionType=None,isSubjectToMaintenance=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.maintenanceActionDate = maintenanceActionDate
        self.maintenanceActionDescription = maintenanceActionDescription
        self.maintenanceActionFrequency = maintenanceActionFrequency
        self.maintenanceActionIsPeriodic = maintenanceActionIsPeriodic
        self.maintenanceActionName = maintenanceActionName
        self.hasMaintenanceActionType = hasMaintenanceActionType
        self.isSubjectToMaintenance = isSubjectToMaintenance
        
        
class AggregationFunction(BIGGObjects):
    __rdf_type__ = ['Bigg.AggregationFunction', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class KPIType(BIGGObjects):
    __rdf_type__ = ['Bigg.KPIType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class ModelStorageInfrastructure(BIGGObjects):
    __rdf_type__ = ['Bigg.ModelStorageInfrastructure', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class ModelType(BIGGObjects):
    __rdf_type__ = ['Bigg.ModelType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class AnalyticalGroup(BIGGObjects):
    __rdf_type__ = ['Bigg.AnalyticalGroup', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,assessesAggregatedKPI=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.assessesAggregatedKPI = assessesAggregatedKPI
        
        
class KeyPerformanceIndicator(BIGGObjects):
    __rdf_type__ = ['Bigg.KeyPerformanceIndicator', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,hasKPIType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.hasKPIType = hasKPIType
        
        
class KPICalculationItem(BIGGObjects):
    __rdf_type__ = ['Bigg.KPICalculationItem', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,assessesSingleKPI=None,groupsForAnalytics=None,hasAnalyticalModel=None,hasKPI=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.assessesSingleKPI = assessesSingleKPI
        self.groupsForAnalytics = groupsForAnalytics
        self.hasAnalyticalModel = hasAnalyticalModel
        self.hasKPI = hasKPI
        
        
class AnalyticalModel(BIGGObjects):
    __rdf_type__ = ['Bigg.AnalyticalModel', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,modelBaselineYear=None,modelLocation=None,modelName=None,modelTrainedDate=None,hasModelStorageInfrastructure=None,hasModelType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.modelBaselineYear = modelBaselineYear
        self.modelLocation = modelLocation
        self.modelName = modelName
        self.modelTrainedDate = modelTrainedDate
        self.hasModelStorageInfrastructure = hasModelStorageInfrastructure
        self.hasModelType = hasModelType
        
        
class EnergySavingType(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergySavingType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class EnergySavingVerificationSource(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergySavingVerificationSource', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class NonEnergyBenefitImpactEvaluation(BIGGObjects):
    __rdf_type__ = ['Bigg.NonEnergyBenefitImpactEvaluation', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class NonEnergyBenefitImpactValueUnit(BIGGObjects):
    __rdf_type__ = ['Bigg.NonEnergyBenefitImpactValueUnit', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class NonEnergyBenefitType(BIGGObjects):
    __rdf_type__ = ['Bigg.NonEnergyBenefitType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class ProjectInvestmentCurrency(BIGGObjects):
    __rdf_type__ = ['Bigg.ProjectInvestmentCurrency', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class ProjectMotivation(BIGGObjects):
    __rdf_type__ = ['Bigg.ProjectMotivation', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class NonEnergyBenefitProducingItem(BIGGObjects):
    __rdf_type__ = ['Bigg.NonEnergyBenefitProducingItem', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,producesNonEnergyBenefit=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.producesNonEnergyBenefit = producesNonEnergyBenefit
        
        
class SavingProducingItem(BIGGObjects):
    __rdf_type__ = ['Bigg.SavingProducingItem', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,producesSaving=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.producesSaving = producesSaving
        
        
class NonEnergyBenefit(BIGGObjects):
    __rdf_type__ = ['Bigg.NonEnergyBenefit', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,nonEnergyBenefitImpactValue=None,nonEnergyBenefitImpactValueDescription=None,nonEnergyBenefitImpactValueVerifiedAndMeasured=None,nonEnergyBenefitImpactVerificationMethod=None,hasNonEnergyBenefitImpactEvaluation=None,hasNonEnergyBenefitImpactValueUnit=None,hasNonEnergyBenefitType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.nonEnergyBenefitImpactValue = nonEnergyBenefitImpactValue
        self.nonEnergyBenefitImpactValueDescription = nonEnergyBenefitImpactValueDescription
        self.nonEnergyBenefitImpactValueVerifiedAndMeasured = nonEnergyBenefitImpactValueVerifiedAndMeasured
        self.nonEnergyBenefitImpactVerificationMethod = nonEnergyBenefitImpactVerificationMethod
        self.hasNonEnergyBenefitImpactEvaluation = hasNonEnergyBenefitImpactEvaluation
        self.hasNonEnergyBenefitImpactValueUnit = hasNonEnergyBenefitImpactValueUnit
        self.hasNonEnergyBenefitType = hasNonEnergyBenefitType
        
        
class EnergySaving(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergySaving', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,energySavingEndDate=None,energySavingIndependentlyVerified=None,energySavingStartDate=None,energySavingValue=None,hasEnergySavingType=None,hasEnergySavingVerificationSource=None,influencesObjective=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.energySavingEndDate = energySavingEndDate
        self.energySavingIndependentlyVerified = energySavingIndependentlyVerified
        self.energySavingStartDate = energySavingStartDate
        self.energySavingValue = energySavingValue
        self.hasEnergySavingType = hasEnergySavingType
        self.hasEnergySavingVerificationSource = hasEnergySavingVerificationSource
        self.influencesObjective = influencesObjective
        
        
class SystemType(BIGGObjects):
    __rdf_type__ = ['Bigg.SystemType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class ZoneType(BIGGObjects):
    __rdf_type__ = ['Bigg.ZoneType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        
        
class Group(BIGGObjects):
    __rdf_type__ = ['Bigg.Group', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,groupName=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.groupName = groupName
        
        
class EnergyPerformanceCertificateAdditionalInfo(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergyPerformanceCertificateAdditionalInfo', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,averageFacadeTransmittance=None,averageWindowsTransmittance=None,biomassSystemPresence=None,buildingTechnicalInspectionCode=None,constructionRegulation=None,districtHeatingOrCoolingConnection=None,electricVehicleChargerPresence=None,geothermalSystemPresence=None,regulationValueForFacadeTransmittance=None,regulationValueForWindowsTransmittance=None,solarPVSystemPresence=None,solarThermalSystemPresence=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.averageFacadeTransmittance = averageFacadeTransmittance
        self.averageWindowsTransmittance = averageWindowsTransmittance
        self.biomassSystemPresence = biomassSystemPresence
        self.buildingTechnicalInspectionCode = buildingTechnicalInspectionCode
        self.constructionRegulation = constructionRegulation
        self.districtHeatingOrCoolingConnection = districtHeatingOrCoolingConnection
        self.electricVehicleChargerPresence = electricVehicleChargerPresence
        self.geothermalSystemPresence = geothermalSystemPresence
        self.regulationValueForFacadeTransmittance = regulationValueForFacadeTransmittance
        self.regulationValueForWindowsTransmittance = regulationValueForWindowsTransmittance
        self.solarPVSystemPresence = solarPVSystemPresence
        self.solarThermalSystemPresence = solarThermalSystemPresence
        
        
class EnergyPerformanceCertificate(BIGGObjects):
    __rdf_type__ = ['Bigg.EnergyPerformanceCertificate', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,C02EmissionsClass=None,annualC02Emissions=None,annualCoolingCO2Emissions=None,annualCoolingEnergyDemand=None,annualCoolingPrimaryEnergyConsumption=None,annualEnergyCost=None,annualFinalEnergyConsumption=None,annualHeatingCO2Emissions=None,annualHeatingEnergyDemand=None,annualHeatingPrimaryEnergyConsumption=None,annualHotWaterCO2Emissions=None,annualHotWaterPrimaryEnergyConsumption=None,annualLightingCO2Emissions=None,annualPrimaryEnergyConsumption=None,coolingCO2EmissionsClass=None,coolingEnergyDemandClass=None,coolingPrimaryEnergyClass=None,energyPerformanceCertificateCertificationMotivation=None,energyPerformanceCertificateCertificationTool=None,energyPerformanceCertificateClass=None,energyPerformanceCertificateDateOfAssessment=None,energyPerformanceCertificateDateOfCertification=None,energyPerformanceCertificateProcedureType=None,energyPerformanceCertificateReferenceNumber=None,heatingCO2EmissionsClass=None,heatingEnergyDemandClass=None,heatingPrimaryEnergyClass=None,hotWaterCO2EmissionsClass=None,hotWaterPrimaryEnergyClass=None,lightingCO2EmissionsClass=None,lightingPrimaryEnergyClass=None,lightingPrimaryEnergyConsumption=None,hasAdditionalInfo=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.C02EmissionsClass = C02EmissionsClass
        self.annualC02Emissions = annualC02Emissions
        self.annualCoolingCO2Emissions = annualCoolingCO2Emissions
        self.annualCoolingEnergyDemand = annualCoolingEnergyDemand
        self.annualCoolingPrimaryEnergyConsumption = annualCoolingPrimaryEnergyConsumption
        self.annualEnergyCost = annualEnergyCost
        self.annualFinalEnergyConsumption = annualFinalEnergyConsumption
        self.annualHeatingCO2Emissions = annualHeatingCO2Emissions
        self.annualHeatingEnergyDemand = annualHeatingEnergyDemand
        self.annualHeatingPrimaryEnergyConsumption = annualHeatingPrimaryEnergyConsumption
        self.annualHotWaterCO2Emissions = annualHotWaterCO2Emissions
        self.annualHotWaterPrimaryEnergyConsumption = annualHotWaterPrimaryEnergyConsumption
        self.annualLightingCO2Emissions = annualLightingCO2Emissions
        self.annualPrimaryEnergyConsumption = annualPrimaryEnergyConsumption
        self.coolingCO2EmissionsClass = coolingCO2EmissionsClass
        self.coolingEnergyDemandClass = coolingEnergyDemandClass
        self.coolingPrimaryEnergyClass = coolingPrimaryEnergyClass
        self.energyPerformanceCertificateCertificationMotivation = energyPerformanceCertificateCertificationMotivation
        self.energyPerformanceCertificateCertificationTool = energyPerformanceCertificateCertificationTool
        self.energyPerformanceCertificateClass = energyPerformanceCertificateClass
        self.energyPerformanceCertificateDateOfAssessment = energyPerformanceCertificateDateOfAssessment
        self.energyPerformanceCertificateDateOfCertification = energyPerformanceCertificateDateOfCertification
        self.energyPerformanceCertificateProcedureType = energyPerformanceCertificateProcedureType
        self.energyPerformanceCertificateReferenceNumber = energyPerformanceCertificateReferenceNumber
        self.heatingCO2EmissionsClass = heatingCO2EmissionsClass
        self.heatingEnergyDemandClass = heatingEnergyDemandClass
        self.heatingPrimaryEnergyClass = heatingPrimaryEnergyClass
        self.hotWaterCO2EmissionsClass = hotWaterCO2EmissionsClass
        self.hotWaterPrimaryEnergyClass = hotWaterPrimaryEnergyClass
        self.lightingCO2EmissionsClass = lightingCO2EmissionsClass
        self.lightingPrimaryEnergyClass = lightingPrimaryEnergyClass
        self.lightingPrimaryEnergyConsumption = lightingPrimaryEnergyConsumption
        self.hasAdditionalInfo = hasAdditionalInfo
        
        
class ResourceType(BIGGObjects):
    __rdf_type__ = ['Bigg.ResourceType', 'Bigg.Thing']

    def __init__(self, subject, comment=None,label=None,relatedDeviceType=None,relatedMeasuredProperty=None,relatedUtilityType=None):
        super().__init__(subject)
        self.comment = comment
        self.label = label
        self.relatedDeviceType = relatedDeviceType
        self.relatedMeasuredProperty = relatedMeasuredProperty
        self.relatedUtilityType = relatedUtilityType
        