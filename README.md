ecobackcast
===========

Experimental tools for back-casting Principal European Economic Indicators.
---

**About**

The code source files provided herein implement the improved method (through hierarchical adjustment) used for the back-calculation, or back-casts, of Principal European Economic Indicators (PEEIs), as described in this [document](docs/R_backcastingPEEIs.pdf). 

<table align="center">
    <tr> <td align="left"><i>authors</i></td> <td align="left"> <a href="https://www.linkedin.com/in/jamieleighchapman">J.-L.Chapman</a>, 
	<a href="https://www.linkedin.com/in/rebecca-killick-0427b615a">R.Killick</a> </tr> 
    <tr> <td align="left"><i>version</i></td> <td align="left">1.0</td> </tr> 
    <tr> <td align="left"><i>since</i></td> <td align="left">2018</td> </tr> 
    <tr> <td align="left"><i>license</i></td> <td align="left"><a href="https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdfEUPL">EUPL</a> <i>(cite the source code or the methodological document above!)</i></td> </tr> 
</table>

**Description**

Note that the following requests were made as part of the methodological enquiry:

* All the available actual observations of all time-series must be used.
* The use of statistically sound, sufficiently detailed documentation, publically available and easily
replicable methodology.
* The methodology should be applicable both to seasonally adjusted and non-seasonally adjusted data.
* The methodology should be flexible and enough easily to incorporate new time-series produced by
Member States.
* The horizons for the back-calculation must be defined accordingly to the amount of the available data,
acceptable back-forecasting (back-casting/back-calculation) error and interpretability of the results.

The source code is available under the [**_src/_**](src) directory. Experimental data used throughout the different programs are stored under the [**_data/_**](src) directory.
