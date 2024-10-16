<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:template name="otherentity" match="/">
    <xsl:choose>
        <xsl:when test=".//otherEntity">
            <h3>Other entities</h3>
            <xsl:for-each select=".//otherEntity">
             <br/>
            <xsl:value-of select="entityName"/>
             <br/>
            </xsl:for-each>  
        </xsl:when>
    </xsl:choose>
</xsl:template>
</xsl:stylesheet>


