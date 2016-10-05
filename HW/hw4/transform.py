fh = open("age.guessing.csv")
nrow = 0
true_ages = ['29', '30', '14', '42', '37', '57', '73', '24', '82', '45']
print("\t".join(("y", "group", "person", "true-age")))
for line in fh:
    nrow += 1
    if nrow == 1 or nrow == 12:
        continue #header
    line = line.rstrip("\n")
    fields = line.split(",")
    ncol = 0
    for index in xrange(2, len(fields)-3):
        ncol += 1
        print("\t".join((fields[index], str(nrow - 1), str(ncol), true_ages[ncol - 1])))
