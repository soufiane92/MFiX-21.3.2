import csv
import json
import os

KEYS = [
    'name',
    'id',
    'smd (m)',
    'size_min (m)',
    'size_max (m)',
    'sphericity',
    'porosity',
    'umf (m/s)',
    'density (kg/m3)',
    'skeletal (kg/m3)',
    'fluffed density (kg/m3)',
    'fluffed void',
    'packed density (kg/m3)',
    'packed void',
    'group',
    ]

CONVERT = [None]*len(KEYS)
CONVERT[2:5] = [1e-6]*3  # um to m
CONVERT[7] = 0.01        # cm/s to m/s
CONVERT[8:11] = [1000]*3 # g/cm3 to kg/m3
CONVERT[12] = 1000       # g/cm3 to kg/m3

class PrettyFloat(float):
    def __repr__(self):
        return '{0:.4g}'.format(self)

def clean_val(s):
    try:
        return PrettyFloat(s)
    except:
        if len(s) == 0:
            return None
        else:
            return s

def convert(val, i):
    f = CONVERT[i]

    if f is not None and val is not None:
        return val*f
    else:
        return val

def float_representer(dumper, value):
    text = '{0:.4g}'.format(value)
    return dumper.represent_scalar(u'tag:yaml.org,2002:float', text)

def read_csv(csv_fname):
    """given a csv file from the website
    (https://mfix.netl.doe.gov/experimentation/granular-materials-database/interactive-gmdb/),
    convert to a SI yaml file"""
    database = {}
    with open(csv_fname, encoding="utf-8", newline="") as csvfile:
        csvfile.readline()
        csvreader = csv.reader(csvfile, delimiter=',')
        for row in csvreader:
            data = dict((key, convert(clean_val(val), i)) for i, (key, val) in enumerate(zip(KEYS, row)))
            if data.get('smd (m)') and data.get('density (kg/m3)'):
                database[row[1]] = data
    return database


def csv_to_yaml(csv_fname, yaml_fname):
    database = read_csv(csv_fname)

    import yaml

    yaml.add_representer(float, float_representer)

    with open(yaml_fname, "w", encoding="utf-8") as yamlfile:
        yaml.dump(database, yamlfile, default_flow_style=False)


def csv_to_json(csv_fname, json_fname):
    database = read_csv(csv_fname)
    with open(json_fname, "w", encoding="utf-8") as jsonfile:
        json.dump(database, jsonfile)


def yaml_to_json(yaml_fname, json_fname):
    import yaml

    with open(yaml_fname, "r", encoding="utf-8") as yamlfile:
        database = yaml.load(yamlfile)
    with open(json_fname, "w", encoding="utf-8") as jsonfile:
        json.dump(database, jsonfile)


if __name__ == '__main__':

    # csv file is exported from the MFiX website
    # Should have a header like:
    # Material Name, ID, SMD(µm), Size_Min(µm), Size_Max(µm), Sphericity(-),
    # Umf(cm/s), Particle Density(g/cm³), Skeletal Density(g/cm³),
    # Fluffed Bulk Density(g/cm³), Fluffed Voidage(-),
    # Packed Bulk Density(g/cm³), Packed Voidage(-), Geldart Group

    csv_path = './materials.csv'
    yaml_path = './particle_props.yaml'
    json_path = './particle_props.json'

    csv_to_yaml(os.path.abspath(csv_path), yaml_path)
    # csv_to_json(os.path.abspath(csv_path), json_path)
    yaml_to_json(yaml_path, json_path)
