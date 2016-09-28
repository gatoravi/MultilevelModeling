counties_samples = {}
fh = open("dat/mn-radon.txt")
for line in fh:
    fields = line.split()
    county = fields[4]
    if county not in counties_samples:
        counties_samples[county] = 0
    counties_samples[county] += 1
fh = open("dat/mn-radon.txt")
for line in fh:
    line = line.rstrip("\n")
    fields = line.split()
    county = fields[4]
    print(line + " " + str(counties_samples[county]))
