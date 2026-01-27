import argparse

def syncFifoW( field ):
    print()
    print(f'U_Config_{field} : entity surf.SynchronizerFifo')
    print(f'  generic map (')
    print(f'    DATA_WIDTH_G => config.{field}\'length )')
    print(f'  port map (')
    print(f'    wr_clk => axilClk,')
    print(f'    din    => r.config.{field},')
    print(f'    rd_clk => clk,')
    print(f'    dout   => config.{field});')
    
def syncFifoR( field ):
    print()
    print(f'U_Status_{field} : entity surf.SynchronizerFifo')
    print(f'  generic map (')
    print(f'    DATA_WIDTH_G => status.{field}\'length )')
    print(f'  port map (')
    print(f'    wr_clk => clk,')
    print(f'    din    => status.{field},')
    print(f'    rd_clk => axilClk,')
    print(f'    dout   => statusS.{field});')
    
def syncVectorR( field ):
    print()
    print(f'U_Status_{field} : entity surf.SynchronizerVector')
    print(f'  generic map (')
    print(f'    WIDTH_G => status.{field}\'length )')
    print(f'  port map (')
    print(f'    clk      => axilClk,')
    print(f'    dataIn   => status.{field},')
    print(f'    doutOut  => statusS.{field});')
    
def main():

    fields = ['clock_divisor','clock_remainder','clock_step','baseDivisor',
              'pulseId', 'timeStamp', 'pulseIdWrEn', 'timeStampWrEn','interval']

    for f in fields:
        syncFifoW( f )

    fields = ['pulseId', 'timeStamp', 'count186M', 'countBRT',]
    for f in fields:
        syncFifoR( f )

    fields = ['nbeamseq', 'nexptseq', 'seqaddrlen', 'nallowseq',
              'pllChanged', 'countSyncE',]
    for f in fields:
        syncVectorR( f )
        
if __name__ == '__main__':
    main()
    
