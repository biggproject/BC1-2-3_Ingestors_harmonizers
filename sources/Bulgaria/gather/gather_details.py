from string import ascii_uppercase


def gather_contacts(wb):
    var = wb['B2'].value.split('/')
    epc = var[0].strip()  # EnergyPerformanceContract.contractID
    epc_date = var[1].strip()
    valid_until = wb['B3'].value  # EnergyPerformanceContract.contractEndDate
    building_type = wb['C14'].value  # Building.buildingType
    energy_class_before_ee_measures = wb['C17'].value  # EnergyPerformanceCertificate.energyPerformanceClass
    energy_class_after_ee_measures = wb['D17'].value  # EnergyPerformanceCertificate.energyPerformanceClass
    energy_consumption_before_ee_measures = wb['C18'].value  # EnergyPerformanceCertificate.annualFinalEnergyConsumption
    energy_consumption_after_ee_measures = wb['D18'].value  # EnergyPerformanceCertificate.annualFinalEnergyConsumption
    organization_type = wb['C19'].value  # Organization.organizationType
    organization_contact_info = wb['C20'].value

    cadastral_ref = wb['C21'].value  # CadastralReference
    municipality = wb['C23'].value  # Location.municipality
    town = wb['C24'].value  # Location.city
    commissioning_date = wb['C25'].value
    floor_area = wb['C26'].value
    gross_floor_area = wb['C27'].value
    heating_area = wb['C28'].value
    heating_volume = wb['C29'].value
    cooling_area = wb['C30'].value
    cooling_volume = wb['C31'].value
    number_of_floors_before = wb['C32'].value
    number_of_floors_after = wb['D32'].value
    number_of_inhabitants = wb['C33'].value

    return {"epc": {"id": epc,
                    "date": epc_date,
                    "valid_until": valid_until,
                    "energy_class_before": energy_class_before_ee_measures,
                    "energy_class_after": energy_class_after_ee_measures,
                    "specific_energy_consumption_before (kWh/m2)": energy_consumption_before_ee_measures,
                    "specific_energy_consumption_after (kWh/m2)": energy_consumption_after_ee_measures},

            "building_type": building_type,
            "organization": {
                "type": organization_type,
                "contact_info": organization_contact_info
            },

            "commissioning_date": commissioning_date,
            "location": {
                "municipality": municipality,
                "town": town
            },
            "area": {
                "gross_floor_area": gross_floor_area,
                "floor_area": floor_area,
                "heating_area": heating_area,
                "heating_volume": heating_volume,
                "cooling_area": cooling_area,
                "cooling_volume": cooling_volume,
                "number_of_floors_before": number_of_floors_before,
                "number_of_floors_after": number_of_floors_after,
                "number_of_inhabitants": number_of_inhabitants
            },
            "cadastral_reference": cadastral_ref

            }


def gather_building_description(wb):
    climate_zone = wb['B3'].value
    type_of_construction = wb['B8'].value

    return {"climate_zone": climate_zone, "type": type_of_construction}


def excel_matrix(init_row, init_alphabet, end_alphabet, header_names, rows_names, wb, id):
    res = []
    for index2 in range(init_row, init_row + len(rows_names)):
        aux = {"type": rows_names[index2 - init_row], "id": id}
        for index, letter in enumerate(ascii_uppercase[init_alphabet:end_alphabet]):
            aux.update({header_names[index]: wb[f"{letter}{index2}"].value})
        res.append(aux)
    return res


def gather_consumption(wb, id):
    header_names = "t	Nm3	kWh	kWh/t  kWh/Nm3	BGN/ton BGN/Nm3	BGN/kWh".split("\t")
    rows_names = ["Heavy fuel oil",
                  "Diesel oil",
                  "LPG",
                  "Diesel oil 2",
                  "Natural gas",
                  "Coal",
                  "Pellets",
                  "Wood",
                  "Other (specify)",
                  "Heat energy",
                  "Electricity"
                  ]
    consumptions = excel_matrix(11, 2, 8, header_names, rows_names, wb, id)

    total_consumption = wb['E22'].value

    consumptions.append({"id": id, "type": 'Total Consumption', "kWh": total_consumption})

    header_names = ["Actual Specific", "Actual Total", "Corrected Specific", "Corrected Total", "Expected Specific",
                    'Expected Total']
    rows_names = ["Heating",
                  "Ventilation",
                  "DHW",
                  "Fans and pumps",
                  "Lighting",
                  "Appliances",
                  "Cooling",
                  "Total",
                  ]

    distribution = excel_matrix(29, 2, 8, header_names, rows_names, wb, id)

    return consumptions, distribution


def gather_savings(wb, id):
    measurements = []
    header_names = "t/a	Nm3/a.	kWh/a.	BGN/a	BGN	year	CO2 t/a".split('\t')
    rows_names = ["Heavy fuel oil",
                  "Diesel oil",
                  "LPG",
                  "Diesel oil 2",
                  "Natural gas",
                  "Coal",
                  "Pellets",
                  "Wood",
                  "Other (specify)",
                  "Heat energy",
                  "Electricity",
                  "Total Measure"
                  ]

    init_row = 7
    for it in range(5):
        init = init_row + (len(rows_names) * it)
        measurements += excel_matrix(init, 4, 11, header_names, rows_names, wb, f"{id}~{it + 1}")

    init = 71
    measurements += excel_matrix(init, 4, 11, header_names, rows_names, wb, f"{id}~{6}")

    init_row = 86
    for it in range(4):
        init = init_row + (len(rows_names) * it)
        measurements += excel_matrix(init, 4, 11, header_names, rows_names, wb, f"{id}~{it + 7}")

    init_row = 137
    for it in range(4):
        init = init_row + (len(rows_names) * it)
        measurements += excel_matrix(init, 4, 11, header_names, rows_names, wb, f"{id}~{it + 11}")

    total_annual_savings = excel_matrix(init, 4, 11, header_names, rows_names, wb, f"{id}~totalAnnualSavings")

    energy_saved = {"total_energy_saved": wb['G204'].value, "shared_energy_saved": wb['G206'].value, "id": id}
    return energy_saved, total_annual_savings, measurements


def transform_data(data):
    general_info = data['general_info']
    consumption = data['consumption']
    distribution = data['distribution']
    measurements = data['measurements']
    total_annual_savings = data['total_annual_savings']

    row = {}
    row.update(**general_info)
    row.update({"epc_id": data['epc_id']})

    for i in range(len(consumption)):
        if consumption[i]["kWh"] is not None:
            row.update({f"consumption_{i}": consumption[i]["kWh"],
                        f"consumption_{i}_type": consumption[i]["type"]
                        })

    for i in range(len(distribution)):
        row.update(
            {f"distribution_{i}": distribution[i]['Actual Total'], f"distribution_{i}_type": distribution[i]['type']})

    for i in range(len(measurements)):
        val = measurements[i]['id'].split('~')[1]
        row.update({f"measure_{int(val) - 1}_{i % 12}": measurements[i]['kWh/a.'],
                    f"measure_{int(val) - 1}_{i % 12}_type": measurements[i]['type']})

    for i in range(len(total_annual_savings)):
        if total_annual_savings[i]['kWh/a.'] is not None:
            row.update({f"total_annual_savings_{i}": total_annual_savings[i]['kWh/a.'],
                        f"total_annual_savings_{i}_type": total_annual_savings[i]['type']})

    return [row]
