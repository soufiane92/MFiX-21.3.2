# Common to fluid and solid phases


class SpeciesHandler:
    def species_all_aliases(self):
        yield from self.fluid_species.keys()
        for sp in self.solids_species.values():
            yield from sp.keys()


    def find_species_phase(self, species):
        # case insensitive
        slower = species.lower()
        for p in range(0, 1+len(self.solids)):
            for s in self.species_of_phase(p):
                if s.lower() == slower:
                    return p
        self.warning("Species %s is not associated with any phase" % species)


    def species_mol_weight(self, species):
        if species in self.fluid_species:
            return self.fluid_species[species].get('mol_weight')
        for (p, subdict) in self.solids_species.items():
            if species in subdict:
                return subdict[species].get('mol_weight')

        return None


    def species_of_phase(self, p):
        if p is None or p > len(self.solids):
            return []
        elif p == 0:
            return self.fluid_species.keys()
        else:
            return self.solids_species[p].keys()


    # TODO move to project_manager
    def species_burcat_name(self, alias, phase, index):
        # Return a name of at most 18 chars that is unique
        # among all species_s and species_g keys
        # Assume 'alias' is already a valid alias, ie no whitespace, etc

        # Collect all species_s and species_g keys except for
        #  specified phase/index
        species_keys = set()
        for p in range(1+len(self.solids)):
            key = 'species_g' if p==0 else 'species_s'
            nmax = len(self.fluid_species if p==0 else self.solids_species[p])
            for j in range(1, nmax+1):
                if p == phase and j == index:
                    continue
                species_keys.add(self.project.get_value(
                    key, default='', args=([p,j] if p else [j])).lower())
        name = alias[:18]
        if name.lower() not in species_keys:
            return name
        count = 1
        base = name[:14]
        while name.lower() in species_keys:
            name = '%s_%s' % (base, count)
            count += 1
        return name
