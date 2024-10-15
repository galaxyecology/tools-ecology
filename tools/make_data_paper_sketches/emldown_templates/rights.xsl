<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template name="rights" match="/">
 <xsl:choose>
 <xsl:when test=".//intellectualRights">
 <h3>Intellectual Rights</h3>
  <p>
    <xsl:value-of select="//dataset/intellectualRights" />
  </p>
 </xsl:when>
</xsl:choose>
</xsl:template>
</xsl:stylesheet>
  
