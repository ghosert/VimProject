<?xml version="1.0"?><!-- this is an xml document -->
<!-- declare the xsl namespace to distinguish xsl tags from html tags -->
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>
<!-- When we see the root element, output the HTML framework of a table -->
<xsl:template match="/">
<table>
<tr><th>Name</th><th>E-mail Address</th></tr>
<xsl:apply-templates/> <!-- and recurse for other templates -->
</table>
</xsl:template>
<!-- When we see a <contact> element... -->
<xsl:template match="contact">
<tr> <!-- Begin a new row of the table -->
<!-- Use the name attribute of the contact as the first column -->
<td><xsl:value-of select="@name"/></td>
<xsl:apply-templates/> <!-- and recurse for other templates -->
</tr>
</xsl:template>
<!-- When we see an <email> element, output its content in another cell -->
<xsl:template match="email">
<td><xsl:value-of select="."/></td>
</xsl:template>
</xsl:stylesheet>

