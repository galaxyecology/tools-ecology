<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:template name="taxonomic_coverage" match="/">
    <xsl:choose>
        <xsl:when test=".//dataset/coverage/taxonomicCoverage">
    <h3>Taxonomic coverage</h3>
        <div class="table-responsive">
            <table class="table table-striped">
                <tr>
					<th>Rank</th>
                    <th>Value</th>
                </tr>
      			<xsl:for-each select="//dataset/coverage/taxonomicCoverage/taxonomicClassification">
					<xsl:call-template name="loop"/>
				</xsl:for-each>
            </table>
        </div>
           </xsl:when>
    </xsl:choose>
    </xsl:template>
	<xsl:template name="loop" match="/">
		<xsl:for-each select="taxonomicClassification">
			<xsl:choose>
				<xsl:when test="descendant::taxonomicClassification">
					<xsl:call-template name="loop"/>
				</xsl:when>
				<xsl:otherwise>
					<tr>
						<td><xsl:value-of select="taxonRankName"/></td>
						<td><xsl:value-of select="taxonRankValue"/></td>
					</tr>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
    </xsl:template>
</xsl:stylesheet>




