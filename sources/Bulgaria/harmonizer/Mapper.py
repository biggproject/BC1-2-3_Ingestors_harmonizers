from slugify import slugify

from sources.Bulgaria.constants import eem_headers, enum_energy_saving_type, \
    enum_energy_efficiency_measurement_type
from utils.data_transformations import to_object_property
from ontology.bigg_classes import Organization, Building, LocationInfo, BuildingSpace, Area, \
    EnergyPerformanceCertificate, BuildingSpaceUseType, AreaType, AreaUnitOfMeasurement, Device, \
    EnergyEfficiencyMeasure, Sensor, EnergySaving, BuildingConstructionElement, RenovationProject, UtilityPointOfDelivery
from ontology.namespaces_definition import Bigg, units, bigg_enums, countries


class Mapper(object):
    def __init__(self, source, namespace):
        self.source = source
        self.eem_list = None
        self.eem_start_column = None
        self.saving_list = None
        self.saving_start_column = None
        Organization.set_namespace(namespace)
        Building.set_namespace(namespace)
        LocationInfo.set_namespace(namespace)
        BuildingSpace.set_namespace(namespace)
        BuildingSpaceUseType.set_namespace(namespace)
        Area.set_namespace(namespace)
        EnergyPerformanceCertificate.set_namespace(namespace)
        AreaType.set_namespace(namespace)
        AreaUnitOfMeasurement.set_namespace(namespace)
        BuildingConstructionElement.set_namespace(namespace)
        Device.set_namespace(namespace)
        UtilityPointOfDelivery.set_namespace(namespace)
        EnergyEfficiencyMeasure.set_namespace(namespace)
        Sensor.set_namespace(namespace)
        EnergySaving.set_namespace(namespace)
        RenovationProject.set_namespace(namespace)

    def select_chunk(self, eem_list, eem_start_column, saving_list, saving_start_column):
        self.eem_list = eem_list
        self.eem_start_column = eem_start_column
        self.saving_list = saving_list
        self.saving_start_column = saving_start_column

    def generate_energy_efficiency_measurement(self, column, links):
        return {
            "name": f"eem_{column}",
            "class": EnergyEfficiencyMeasure,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "hasEnergyEfficiencyMeasureInvestmentCurrency": units["BulgarianLev"],
                    "energyEfficiencyMeasureCurrencyExchangeRate": "0.51",
                    "hasEnergyEfficiencyMeasureType": bigg_enums[enum_energy_efficiency_measurement_type[column]],
                },
                "mapping": {
                    "subject": {
                        "key": f"eem_{column}_subject",
                        "operations": []
                    },
                    "energyEfficiencyMeasureInvestment": {
                        "key": f"measurement_{column}_Investments",
                        "operations": []
                    },
                    "energyEfficiencyMeasureOperationalDate": {
                        "key": f"epc_date",
                        "operations": []
                    }
                },
            },
            "links": links
        }

    def generate_energy_saving(self, column, subcolumn, energy_saving_type, measurement_type):
        return {
            "name": f"energy_saving_{column}_{subcolumn}",
            "class": EnergySaving,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "hasEnergySavingType": bigg_enums[energy_saving_type]
                },
                "mapping": {
                    "subject": {
                        "key": f"energy_saving_{column}_{subcolumn}_subject",
                        "operations": []
                    },
                    "energySavingValue": {
                        "key": f"measurement_{column}_{measurement_type}",
                        "operations": []
                    },
                    "energySavingStartDate": {
                        "key": f"epc_date_before",
                        "operations": []
                    },
                    "energySavingEndDate": {
                        "key": f"epc_date",
                        "operations": []
                    }
                }
            }
        }

    def get_mappings(self, group):
        # building info mapping
        organization = {
            "name": "organization",
            "class": Organization,
            "type": {
                "origin": "static"
            },
            "params": {
                "raw": {
                    "subject": "bulgaria",
                    "organizationName": "Bulgaria"
                }
            },
            "links": {
                "location_organization": {
                    "type": Bigg.hasSubOrganization,
                    "link": "__all__"
                }
            }
        }

        location_organization = {
            "name": "location_organization",
            "class": Organization,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "organizationDivisionType": "Location"
                },
                "mapping": {
                    "subject": {
                        "key": "location_org_subject",
                        "operations": []
                    },
                    "organizationName": {
                        "key": "municipality",
                        "operations": []
                    }
                }
            },
            "links": {
                "building_organization": {
                    "type": Bigg.hasSubOrganization,
                    "link": "organization_subject"
                }
            }
        }

        building_organization = {
            "name": "building_organization",
            "class": Organization,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "organizationDivisionType": "Building"
                },
                "mapping": {
                    "subject": {
                        "key": "organization_subject",
                        "operations": []
                    },
                    "organizationName": {
                        "key": "building_name",
                        "operations": []
                    }
                }
            },
            "links": {
                "buildings": {
                    "type": Bigg.managesBuilding,
                    "link": "subject"
                }
            }
        }

        buildings = {
            "name": "buildings",
            "class": Building,
            "type": {
                "origin": "row"
            },
            "params": {
                "mapping": {
                    "subject": {
                        "key": "building_subject",
                        "operations": []
                    },
                    "buildingName": {
                        "key": "building_name",
                        "operations": []
                    },
                    "buildingIDFromOrganization": {
                        "key": "building_id",
                        "operations": []
                    }
                }
            },
            "links": {
                "building_space": {
                    "type": Bigg.hasSpace,
                    "link": "subject"
                },
                "location_info": {
                    "type": Bigg.hasLocationInfo,
                    "link": "subject"
                },
                "energy_performance_certificate_before": {
                    "type": Bigg.hasEPC,
                    "link": "subject"
                },
                "energy_performance_certificate_after": {
                    "type": Bigg.hasEPC,
                    "link": "subject"
                },
                "project": {
                    "type": Bigg.hasProject,
                    "link": "subject"
                }
            }
        }

        location_info = {
            "name": "location_info",
            "class": LocationInfo,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "hasAddressCountry": countries["732800/"],
                    "addressTimeZone": "Europe/Sofia"
                },
                "mapping": {
                    "subject": {
                        "key": "location_subject",
                        "operations": []
                    },
                    "hasAddressCity": {
                        "key": "hasAddressCity",
                        "operations": []
                    }
                }
            }
        }

        energy_performance_certificate_before = {
            "name": "energy_performance_certificate_before",
            "class": EnergyPerformanceCertificate,
            "type": {
                "origin": "row"
            },
            "params": {
                "mapping": {
                    "subject": {
                        "key": "epc_before_subject",
                        "operations": []
                    },
                    "energyPerformanceCertificateDateOfAssessment": {
                        "key": "epc_date_before",
                        "operations": []
                    },
                    "energyPerformanceCertificateClass": {
                        "key": "epc_energy_class_before",
                        "operations": []
                    },
                    "annualFinalEnergyConsumption": {
                        "key": "annual_energy_consumption_before_total_consumption",
                        "operations": []
                    }
                }
            }
        }

        energy_performance_certificate_after = {
            "name": "energy_performance_certificate_after",
            "class": EnergyPerformanceCertificate,
            "type": {
                "origin": "row"
            },
            "params": {
                "mapping": {
                    "subject": {
                        "key": "epc_after_subject",
                        "operations": []
                    },
                    "energyPerformanceCertificateDateOfAssessment": {
                        "key": "epc_date",
                        "operations": []
                    },
                    "energyPerformanceCertificateClass": {
                        "key": "epc_energy_class_after",
                        "operations": []
                    }
                }
            }
        }

        project = {
            "name": "project",
            "class": RenovationProject,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "hasProjectInvestmentCurrency": units.BulgarianLev
                },
                "mapping": {
                    "subject": {
                        "key": "project_subject",
                        "operations": []
                    },
                    "projectIDFromOrganization": {
                        "key": "subject",
                        "operations": []
                    },
                    "projectStartDate": {
                        "key": "epc_date",
                        "operations": []
                    },
                    "projectInvestment": {
                        "key": "total_savings_Investments",
                        "operations": []
                    },

                }
            },
            "links": {}
        }

        building_space = {
            "name": "building_space",
            "class": BuildingSpace,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "buildingSpaceName": "Building"
                },
                "mapping": {
                    "subject": {
                        "key": "building_space_subject",
                        "operations": []
                    },
                    "hasBuildingSpaceUseType": {
                        "key": "buildingSpaceUseType",
                        "operations": []
                    },
                }
            },
            "links": {
                "gross_floor_area": {
                    "type": Bigg.hasArea,
                    "link": "subject"
                },
                "element": {
                    "type": Bigg.isAssociatedWithElement,
                    "link": "subject"
                },
                "device": {
                    "type": Bigg.isObservedByDevice,
                    "link": "subject"
                },
                "utilityPoint": {
                    "type": Bigg.hasUtilityPointOfDelivery,
                    "link": "subject"
                }
            }
        }

        gross_floor_area = {
            "name": "gross_floor_area",
            "class": Area,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "hasAreaType": bigg_enums["GrossFloorArea"],
                    "hasAreaUnitOfMeasurement": units["M2"]
                },
                "mapping": {
                    "subject": {
                        "key": "gross_floor_area_subject",
                        "operations": []
                    },
                    "areaValue": {
                        "key": "gross_floor_area",
                        "operations": []
                    }
                }
            }
        }

        element = {
            "name": "element",
            "class": BuildingConstructionElement,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "hasBuildingConstructionElementType": bigg_enums["OtherBuildingConstructionElement"],
                },
                "mapping": {
                    "subject": {
                        "key": "element_subject",
                        "operations": []
                    }
                }
            },
            "links": {
                "device": {
                    "type": Bigg.isObservedByDevice,
                    "link": "subject"
                }
            }
        }
        device = {
            "name": "device",
            "class": Device,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "hasDeviceType": to_object_property("Meter.EnergyMeter", namespace=bigg_enums)
                },
                "mapping": {
                    "subject": {
                        "key": "device_subject",
                        "operations": []
                    },
                    "deviceName": {
                        "key": "building_id",
                        "operations": []
                    },
                }
            }
        }

        utilityPoint = {
            "name": "utilityPoint",
            "class": UtilityPointOfDelivery,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "hasUtilityType": to_object_property("Electricity", namespace=bigg_enums)
                },
                "mapping": {
                    "subject": {
                        "key": "utility_subject",
                        "operations": []
                    },
                    "pointOfDeliveryIDFromOrganization": {
                        "key": "building_id",
                        "operations": []
                    },
                }
            },
            "links": {
                "device": {
                    "type": Bigg.hasDevice,
                    "link": "subject"
                }
            }
        }

        # eem_savings mapping
        energy_saving_list = []
        eem_list = []
        if self.eem_list:

            for i, eem_type in enumerate(self.eem_list):
                list_i = i + self.eem_start_column
                links = {}
                for j, saving_type in enumerate(self.saving_list):
                    list_j = j + self.saving_start_column
                    energy_saving_list.append(self.generate_energy_saving(column=list_i, subcolumn=list_j,
                                                                          energy_saving_type=saving_type,
                                                                          measurement_type=eem_headers[list_j]))
                    links.update({f"energy_saving_{list_i}_{list_j}": {"type": Bigg.producesSaving, "link": "subject"}})
                eem_list.append(self.generate_energy_efficiency_measurement(column=list_i, links=links))

            for i in range(len(self.eem_list)):
                list_i = i + self.eem_start_column
                element['links'].update({f"eem_{list_i}": {
                    "type": Bigg.isAffectedByMeasure,
                    "link": "subject"}})

        # project linking and savings
        eem_links_only = [{
            "name": f"eem_{i}",
            "class": EnergyEfficiencyMeasure,
            "type": {
                "origin": "row"
            },
            "params": {
                "mapping": {
                    "subject": {
                        "key": f"eem_{i}_subject",
                        "operations": []
                    }
                }
            }
        } for i in range(len(enum_energy_efficiency_measurement_type))
        ]
        project_energy_saving = [{
            "name": f"project_energy_saving_{enum_energy_saving_type[i]}",
            "class": EnergySaving,
            "type": {
                "origin": "row"
            },
            "params": {
                "raw": {
                    "hasEnergySavingType": bigg_enums[enum_energy_saving_type[i]]
                },
                "mapping": {
                    "subject": {
                        "key": f"project_energy_saving_subject_{enum_energy_saving_type[i]}",
                        "operations": []
                    },
                    "energySavingValue": {
                        "key": f"total_savings_{eem_headers[i]}",
                        "operations": []
                    },
                    "energySavingStartDate": {
                        "key": f"epc_date_before",
                        "operations": []
                    },
                    "energySavingEndDate": {
                        "key": f"epc_date",
                        "operations": []
                    }
                }
            }
        } for i in range(len(enum_energy_saving_type))]

        for i, eem_type in enumerate(enum_energy_efficiency_measurement_type):
            project['links'].update({f"eem_{i}": {
                "type": Bigg.includesMeasure,
                "link": "subject"}})

        for saving_type in enum_energy_saving_type:
            project['links'].update({
                f"project_energy_saving_{saving_type}": {
                    "type": Bigg.producesSaving,
                    "link": "subject"}})


        grouped_modules = {
            "building_info": [organization, location_organization, building_organization, buildings, building_space,
                              gross_floor_area, location_info, energy_performance_certificate_before,
                              energy_performance_certificate_after, element, device, utilityPoint, project],
            "eem_savings": energy_saving_list + eem_list + [element],
            "project_info": [project] + eem_links_only + project_energy_saving,
        }
        return grouped_modules[group]