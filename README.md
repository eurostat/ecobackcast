ecobackcast
===========

Experimental tools for back-casting Principal European Economic Indicators.
---

**About**

The code source files provided herein implement the improved method (through hierarchical adjustment) used for the back-calculation, or back-casts, of Principal European Economic Indicators (PEEIs), as described in this [document](docs/R_backcastingPEEIs.pdf) (see also references [below](#References)). The methodology and source code were developed in the framework of Eurostat's contract on database management and research activities related to the production of PEEIs.

<table align="center">
    <tr> <td align="left"><i>authors</i></td> <td align="left"> <a href="https://www.linkedin.com/in/jamieleighchapman">J.-L.Chapman</a>, 
	<a href="https://www.linkedin.com/in/rebecca-killick-0427b615a">R.Killick</a> </tr> 
    <tr> <td align="left"><i>version</i></td> <td align="left">1.0</td> </tr> 
    <tr> <td align="left"><i>since</i></td> <td align="left">2018</td> </tr> 
    <tr> <td align="left"><i>license</i></td> <td align="left"><a href="https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdfEUPL">EUPL</a> <i>(cite the source code or the methodological document above!)</i></td> </tr> 
</table>

**Description**

This methodology allows to make backcasts over the entire structure of the PEEI time series. This approach gives hierarchical consistency both cross-sectionally and temporally. This means that the backcasts for the countries will sum to the backcasts for the whole euro area. Additionally, the backcasts for quarterly data will sum up to the corresponding backcasts for annual data. The method was implemented in R.

The source code is available under the [**_src/_**](src) directory. Experimental data used throughout the different programs are stored under the [**_data/_**](src) directory.


**<a name="References"></a>References** 

* Chapman, J.-L. and Killick, R. (2018): [**Back-casting Principal European Economic Indicators](https://github.com/eurostat/ecobackcast/blob/master/docs/R_backcastingPEEIs.pdf), Luxembourg.

* Principal European Economic Indicators: [glossary](https://ec.europa.eu/eurostat/statistics-explained/index.php/Glossary:Principal_European_economic_indicators_(PEEI)), [overview](https://ec.europa.eu/eurostat/web/euro-indicators/overview) and [scoreboard](https://ec.europa.eu/eurostat/web/euro-indicators/scoreboard).

* [**Principal European Economic Indicators - A statistical guide**](https://ec.europa.eu/eurostat/documents/3217494/5713943/KS-81-08-398-EN.PDF/b7c1a8d5-2ea1-4e1c-b585-4582e92f5e2d?version=1.0), _Publications Office of the European Union_, 2009.
