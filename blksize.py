#!/usr/bin/env python3

def main():


	dasdPtr := flag.String("dasd", "", "DASD model")
	// default to no DASD
	lreclPtr := flag.Int("lrecl", 0, "logical record length")
	//default to zero lrecl
	helpPtr := flag.Bool("help", false, "help flag")
	// default  to no help
	flag.Parse() //  this parses the command line arguments

	lrecl = *lreclPtr // assign argument to main variable lrecl
	dasd = *dasdPtr   // assign dasd type to main variable dasd

	if dasd == "" { // no dasd type was input
		fmt.Println("\nBLK205R No DASD type entered. ")
		fmt.Println("BLK206R pls enter DASD type or restart with -h for list of DASD types")
		fmt.Scan(&dasd)
	}
	if lrecl == 0 { // no lrecl was input

		fmt.Printf("\nBLK201R lrecl command line argument is not included.\n")
		fmt.Println("BLK202R Please enter lrecl length: ")
		fmt.Scan(&lrecl)
	}
	if *helpPtr { // user asked for help
		fmt.Println("BLK080I Usage example:   ")
		fmt.Println("BLK080I blockfactor -h -dasd=3380K -lrecl=80")
		fmt.Println(" ")
		fmt.Println("-h          show this help         ")
		fmt.Println("-dasd=3380  IBM DASD type (see table below)")
		fmt.Println("-lrecl=80   logical record length")
		fmt.Println("Possible IBM DASD types:")
		fmt.Println("2311 2314 3330 3340 3350 3375 3380 3390 9345")
		return
	}
	// build table with full track size
	table := make(map[string]int)
	table["2311"] = 3625
	table["2314"] = 7294
	table["3330"] = 13030
	table["3340"] = 8368
	table["3350"] = 19069
	table["3375"] = 35616
	table["3380"] = 47476
	table["3390"] = 56664
	table["9345"] = 46456
	/*  this is just an exampmle on how to print out table
		    p.s. prints out in random order.... not sure why
	    	for model, size := range table {
		         fmt.Println("Model",model, "size",size)
		    } */

	/* formula:  BLOCKIZE = INT(half of TRKSZIE/LRECL) * LRECL */

	halftracks := table[dasd] / 2 // half track size
	blocksize := int((halftracks / lrecl) * lrecl)

	fmt.Println("\nBLK100I Ideal blocksize for DASD type", dasd, ", LRECL: ", lrecl, " is: ", blocksize)
	fmt.Println("BLK900I END OF PROCESSING")

	return
}

